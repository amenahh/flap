(** This module implements a compiler from Hobix to Fopix. *)

(** As in any module that implements {!Compilers.Compiler}, the source
    language and the target language must be specified. *)

module Source = Hobix
module S = Source.AST
module Target = Fopix
module T = Target.AST

(**

   The translation from Hobix to Fopix turns anonymous
   lambda-abstractions into toplevel functions and applications into
   function calls. In other words, it translates a high-level language
   (like OCaml) into a first order language (like C).

   To do so, we follow the closure conversion technique.

   The idea is to make explicit the construction of closures, which
   represent functions as first-class objects. A closure is a block
   that contains a code pointer to a toplevel function [f] followed by all
   the values needed to execute the body of [f]. For instance, consider
   the following OCaml code:

   let f =
     let x = 6 * 7 in
     let z = x + 1 in
     fun y -> x + y * z

   The values needed to execute the function "fun y -> x + y * z" are
   its free variables "x" and "z". The same program with explicit usage
   of closure can be written like this:

   let g y env = env[1] + y * env[2]
   let f =
      let x = 6 * 7 in
      let z = x + 1 in
      [| g; x; z |]

   (in an imaginary OCaml in which arrays are untyped.)

   Once closures are explicited, there are no more anonymous functions!

   But, wait, how to we call such a function? Let us see that on an
   example:

   let f = ... (* As in the previous example *)
   let u = f 0

   The application "f 0" must be turned into an expression in which
   "f" is a closure and the call to "f" is replaced to a call to "g"
   with the proper arguments. The argument "y" of "g" is known from
   the application: it is "0". Now, where is "env"? Easy! It is the
   closure itself! We get:

   let g y env = env[1] + y * env[2]
   let f =
      let x = 6 * 7 in
      let z = x + 1 in
      [| g; x; z |]
   let u = f[0] 0 f

   (Remark: Did you notice that this form of "auto-application" is
   very similar to the way "this" is defined in object-oriented
   programming languages?)

*)

(**
   Helpers functions.
*)

let error pos msg =
  Error.error "compilation" pos msg

let make_fresh_variable =
  let r = ref 0 in
  fun () -> incr r; T.Id (Printf.sprintf "_%d" !r)


let make_fresh_function_identifier =
  let r = ref 0 in
  fun () -> incr r; T.FunId (Printf.sprintf "_%d" !r)

let define e f =
  let x = make_fresh_variable () in
  T.Define (x, e, f x)

let rec defines ds e =
  match ds with
    | [] ->
      e
    | (x, d) :: ds ->
      T.Define (x, d, defines ds e)

let seq a b =
  define a (fun _ -> b)

let rec seqs = function
  | [] -> assert false
  | [x] -> x
  | x :: xs -> seq x (seqs xs)

let allocate_block e =
  T.(FunCall (FunId "allocate_block", [e]))

let write_block e i v =
  T.(FunCall (FunId "write_block", [e; i; v]))

let read_block e i =
  T.(FunCall (FunId "read_block", [e; i]))

let lint i =
  T.(Literal (LInt (Int64.of_int i)))


(** [free_variables e] returns the list of free variables that
     occur in [e].*)
let free_variables =
  let module M =
    Set.Make (struct type t = S.identifier let compare = compare end)
  in
  let rec unions f = function
    | [] -> M.empty
    | [s] -> f s
    | s :: xs -> M.union (f s) (unions f xs)
  in
  let rec fvs = function
    | S.Literal _ ->
       M.empty
    | S.Variable x ->
       M.singleton x
    | S.While (cond, e) ->
      unions fvs [cond;e]
    | S.Define (vd, body) ->
      
      let fvs_body = fvs body
      in
      begin
        match vd with
        | S.SimpleValue(id,expression) -> 
          (* x1 x2 *)
          let fvs_expr = fvs expression in
          let body_sans_id = M.remove id fvs_body in
          (* let fvs_remove = M.remove id fvs_expr in si on a let x = x *)
          M.union fvs_expr body_sans_id
          
        | S.RecFunctions(l) -> 
          let lId = List.map fst l in
          let lExpr = List.map snd l in

          let rec fvs_app = function
          |[] -> M.empty
          |expr::t -> M.union (fvs expr) (fvs_app t)

          in
          let initial_env = fvs_app lExpr in 
          let (new_body_env,new_expr_env) = List.fold_left (
            fun (acc_expr,acc_body) (id, _expr) ->
              ( 
                let next_expr = M.remove id acc_expr in 
                let next_body = M.remove id acc_body in 
                (next_expr,next_body)
              ) ) (initial_env,fvs body) l in
          
          M.union new_body_env new_expr_env
      end
       (* failwith "Students! This is your job!" *)
    | S.ReadBlock (a, b) ->
       unions fvs [a; b]
    | S.Apply (a, b) ->
       let listbinop = ["`+`";"`-`";"`*`";"`/`";"`>?`";"`>=?`";"`<?`";"`<=?`";"`=?`";"`&&`";"`||`";"print_int"] in 
        let u = unions fvs (a :: b) in
        List.fold_left (fun acc_union binop -> 
        M.remove (S.Id binop) acc_union) u listbinop 
    | S.WriteBlock (a, b, c) | S.IfThenElse (a, b, c) ->
       unions fvs [a; b; c]
    | S.AllocateBlock a ->
       fvs a
    | S.Fun (xs, e) ->
      let fvs_expr = fvs e in
        List.fold_left (fun acc e -> M.remove e acc) fvs_expr xs
    | S.Switch (a, b, c) ->
       let c = match c with None -> [] | Some c -> [c] in
       unions fvs (a :: ExtStd.Array.present_to_list b @ c)
  in
  fun e -> M.elements (fvs e)

(**

    A closure compilation environment relates an identifier to the way
    it is accessed in the compiled version of the function's
    body.

    Indeed, consider the following example. Imagine that the following
    function is to be compiled:

    fun x -> x + y

    In that case, the closure compilation environment will contain:

    x -> x
    y -> "the code that extract the value of y from the closure environment"

    Indeed, "x" is a local variable that can be accessed directly in
    the compiled version of this function's body whereas "y" is a free
    variable whose value must be retrieved from the closure's
    environment.

*)
type environment = {
    vars : (HobixAST.identifier, FopixAST.expression) Dict.t;
    externals : (HobixAST.identifier, int) Dict.t;
}

let initial_environment () =
  { vars = Dict.empty; externals = Dict.empty }

let bind_external id n env =
  { env with externals = Dict.insert id n env.externals }

let is_external id env =
  Dict.lookup id env.externals <> None

let reset_vars env =
   { env with vars = Dict.empty }

(** Precondition: [is_external id env = true]. *)
let arity_of_external id env =
  match Dict.lookup id env.externals with
    | Some n -> n
    | None -> assert false (* By is_external. *)

let nb_args prim = 
  let binops = ["`+`";"`-`";"`*`";"`/`";"`>?`";"`>=?`";"`<?`";"`<=?`";"`=?`";"`&&`";"`||`"] in 
  match prim with
  | op when List.mem op binops -> 2
  |_ -> 1
  
let primitives prim =
    
    let this = make_fresh_variable () in             
    let id_fun = make_fresh_function_identifier () in 
  
    let liste_args = 
      if (nb_args prim) > 1 then
        [make_fresh_variable();make_fresh_variable()]
      else
        [make_fresh_variable()]
    in

    let toVar = List.map(fun x -> T.Variable x ) liste_args in
    let fun_s = T.DefineFunction(id_fun,this::liste_args,FunCall(T.FunId prim,toVar)) in
    
    fun_s,id_fun
        


(** [translate p env] turns an Hobix program [p] into a Fopix program
    using [env] to retrieve contextual information. *)
let translate (p : S.t) env =
  let rec program env defs =
  (* let binops = ["`+`";"`-`";"`*`";"`/`";"`>?`";"`>=?`";"`<?`";"`<=?`";"`=?`";"`&&`";"`||`";"print_int"]
  in 
  let deflist = List.map (fun b -> primitives b ) binops in  *)
  let env, defs = ExtStd.List.foldmap definition env defs in
    (List.flatten defs, env)
  and definition env = function
    | S.DeclareExtern (id, n) ->
       let env = bind_external id n env in
       (env, [T.ExternalFunction (function_identifier id, n)])
    | S.DefineValue vd ->
       (env, value_definition env vd)
  and value_definition env = function
    | S.SimpleValue (x, e) -> 
       let fs, e = expression (reset_vars env) e in
       fs @ [T.DefineValue (identifier x, e)]
    | S.RecFunctions fdefs ->
     let fs, defs = define_recursive_functions fdefs in 
     fs @ List.map (fun (x, e) -> T.DefineValue (x, e)) defs 
     


and define_recursive_functions rdefs =
    

    let pre_alloc = List.map (fun (name, expr) ->
      match expr with
      | S.Fun (args, e) -> (name, args, e, make_fresh_variable ())
      | _ -> failwith "Erreur pas une fonction"
    ) rdefs in


    let rec_env = List.fold_left (fun acc_env (name, _, _, ptr) ->
      { acc_env with vars = Dict.insert name (T.Variable ptr) acc_env.vars }
    ) env pre_alloc in
   
    let results = List.map (fun (name, args, body, ptr) ->
        
       
        let (expr_fs, fun_s, id_fun, fvs) = toplevel_fun_body rec_env args body in

        
        let taille = List.length fvs + 1 in
        let block_e = allocate_block (lint taille) in

       
        let write_e = write_block (T.Variable ptr) (lint 0) (T.Literal (LFun id_fun)) in
        let list_seq = write_e::List.mapi 
        (fun i var ->
            let valeur = match Dict.lookup var rec_env.vars with
               | Some e -> e 
               | None -> T.Variable (identifier var)
            in write_block (T.Variable ptr) (lint (i + 1)) valeur
        ) fvs in
        
        
        (expr_fs @ [fun_s], (ptr, block_e), list_seq, (identifier name, T.Variable ptr))
    ) pre_alloc in

    
    let all_fs = List.flatten (List.map (fun (fs, _, _, _) -> fs) results) in

    
    let defs_alloc = List.map (fun (_, (p, a), _, _) -> (p, a)) results in
    let defs_names = List.map (fun (_, _, _, name_pair) -> name_pair) results in
    let seq_list = List.flatten (List.map (fun (_, _, s, _) -> s) results) in

    let var = make_fresh_variable () in
    let local_defs = defs_alloc @ [(var, seqs seq_list)] @ defs_names in

    (all_fs, local_defs)


       (* failwith "define recursive" *)
  and expression env = function
    | S.Literal l ->
      [], T.Literal (literal l)
    | S.While (cond, e) ->
       let cfs, cond = expression env cond in
       let efs, e = expression env e in
       cfs @ efs, T.While (cond, e)
    | S.Variable x ->
      (
      let binops = ["`+`";"`-`";"`*`";"`/`";"`>?`";"`>=?`";"`<?`";"`<=?`";"`=?`";"`&&`";"`||`";"print_int"]  in
      
    
      
      match x with 
      |(Id op) when List.mem op binops -> 

        let tdef,id_fun = primitives op in


        let block_e = allocate_block ( lint 1) in
        let pointeur_id = make_fresh_variable () in 
        let pointeur = T.Variable pointeur_id in
       
        let write_e = write_block pointeur (lint 0) (T.Literal (LFun id_fun)) in

        let seq = [block_e;write_e;pointeur] in
        [tdef],T.Define ( pointeur_id, block_e,seqs seq)
      |_ -> 
       
        let xc =
          match Dict.lookup x env.vars with
            | None -> T.Variable (identifier x)
            | Some e -> e
        in
        ([], xc)
      )
      

    | S.Define (vdef, a) ->
      begin
        match vdef with
        | SimpleValue(id,expr) -> 
          let expr_fs , e = expression env expr in
          let new_env = {env with vars = Dict.insert id (T.Variable (identifier id)) env.vars} in
          let afs, a = expression new_env a in
          expr_fs@afs,T.Define(identifier id,e,a)
        | RecFunctions(fdefs) ->
          let fs, local_defs = define_recursive_functions fdefs in 
          let afs, _a = expression env a in
          (fs @ afs, defines local_defs _a)
        end

    | S.Apply (a, bs) ->

      
      let bfs , exrplist = expressions env bs in
      
      
      let binops = ["`+`";"`-`";"`*`";"`/`";"`>?`";"`>=?`";"`<?`";"`<=?`";"`=?`";"`&&`";"`||`";"print_int"]  in
      (
      
      match a with 
      | S.Variable (Id op) when List.mem op binops -> bfs,T.FunCall(FunId(op),exrplist)
      |_ -> 
      let afs,_a = expression env a in
      let ptr = read_block _a (lint 0) in 
      afs@bfs, T.UnknownFunCall (ptr,_a::exrplist)
      )
  
    | S.IfThenElse (a, b, c) ->
      let afs, a = expression env a in
      let bfs, b = expression env b in
      let cfs, c = expression env c in
      afs @ bfs @ cfs, T.IfThenElse (a, b, c)

    | S.Fun (x, e) ->
        
        let (expr_fs, fun_s, id_fun, free_e) = toplevel_fun_body env x e in
        let taille = (List.length free_e) +1 in
        let block_e = allocate_block ( lint taille ) in
        let pointeur_id = make_fresh_variable () in 
        let pointeur = T.Variable pointeur_id in

        let write_e = write_block pointeur (lint 0) (T.Literal (LFun id_fun)) in
        let list_seq = [write_e]@(List.mapi 
        (fun i e -> 
           let var_value = match Dict.lookup e env.vars with
           | Some expr -> expr
           | None -> T.Variable (identifier e) 
        in 
          write_block pointeur (lint (i+1)) var_value) 
        ) free_e 
        @[pointeur]  
        in
        let retour =T.Define ( pointeur_id, block_e,  (seqs list_seq))
        in
        expr_fs@[fun_s], retour

    | S.AllocateBlock a ->
      let afs, a = expression env a in
      (afs, allocate_block a)
    | S.WriteBlock (a, b, c) ->
      let afs, a = expression env a in
      let bfs, b = expression env b in
      let cfs, c = expression env c in
      afs @ bfs @ cfs,
      T.FunCall (T.FunId "write_block", [a; b; c])
    | S.ReadBlock (a, b) ->
      let afs, a = expression env a in
      let bfs, b = expression env b in
      afs @ bfs,
      T.FunCall (T.FunId "read_block", [a; b])
    | S.Switch (a, bs, default) ->
      let afs, a = expression env a in
      let bsfs, bs =
        ExtStd.List.foldmap (fun bs t ->
                    match ExtStd.Option.map (expression env) t with
                    | None -> (bs, None)
                    | Some (bs', t') -> (bs @ bs', Some t')
                  ) [] (Array.to_list bs)
      in
      let dfs, default = match default with
        | None -> [], None
        | Some e -> let bs, e = expression env e in bs, Some e
      in
      afs @ bsfs @ dfs,
      T.Switch (a, Array.of_list bs, default)

  and toplevel_fun_body env x e =
    let free_e = free_variables (S.Fun (x,e)) in
    let this = make_fresh_variable () in         
    
    let (new_env,_) = 
        List.fold_left (fun (acc_env,i) var -> 
          let access = read_block (T.Variable this) (lint i) in
          ({ acc_env with vars = Dict.insert var access acc_env.vars }, i + 1)
          ) (env, 1) free_e
    in     
    
    let  expr_fs , expr = expression new_env e in   
    let id_fun = make_fresh_function_identifier () in 
    let fun_s = T.DefineFunction(id_fun,this::List.map identifier x,expr) in
    (expr_fs, fun_s, id_fun, free_e)

  and expressions env = function
    | [] ->
       [], []
    | e :: es ->
       let efs, es = expressions env es in
       let fs, e = expression env e in
       fs @ efs, e :: es

  and literal = function
    | S.LInt x -> T.LInt x
    | S.LString s -> T.LString s
    | S.LChar c -> T.LChar c

  and identifier (S.Id x) = T.Id x

  and function_identifier (S.Id x) = T.FunId x

  in
  program env p
