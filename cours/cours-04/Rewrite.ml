module type LANG = sig
  type t
  val pp : t Fmt.t
  val equal : t -> t -> bool
  val rewrite : t -> t Seq.t
end

module Make(L : LANG) = struct
  type graph = (L.t * L.t) Seq.t

  let rec reductions t =
    let open Seq in
    let* t' = L.rewrite t in
    cons (t, t') (reductions t')

  let is_normal t =
    L.rewrite t |> Seq.uncons |> Option.is_none

  let normal_forms t =
    let open Seq in
    reductions t |> map snd |> filter is_normal |> Seq.dedup L.equal

  let to_graphviz g =
    let b = Buffer.create 100 in
    let ff = Format.formatter_of_buffer b in

    (* Disable line breaks. *)
    let ff_funs = Format.pp_get_formatter_out_functions ff () in
    Format.pp_set_formatter_out_functions
      ff
      { ff_funs with
        out_newline = (fun _ -> ());
        out_indent = (fun _ -> ());
        out_spaces = (fun _ -> ());
      };

    Format.fprintf ff "strict digraph {\n";
    Format.fprintf ff "  overlap=scale;\n";
    Format.fprintf ff "  splines=true;\n";
    Format.fprintf ff "  rankdir=\"LR\";\n";
    Seq.iter (fun (t, t') ->
        Format.fprintf ff "\"%a\" -> \"%a\";\n"
          L.pp t
          L.pp t') g;
    Format.fprintf ff "}@.";
    Buffer.contents b

  let display ?(verbose = false) ?(max = 20) g =
    let gv_fn, oc = Filename.open_temp_file "red" ".dot" in
    let pdf_fn = Filename.temp_file "graph" ".pdf" in
    output_string oc @@ to_graphviz @@ Seq.take max g;
    close_out oc;
    if verbose then Printf.eprintf "Graph in %s, PDF in %s\n" gv_fn pdf_fn;
    flush stderr;
    ignore @@ Unix.system Printf.(sprintf "dot -Tpdf %s > %s" gv_fn pdf_fn);
    ignore @@ Unix.system Printf.(sprintf "xdg-open %s" pdf_fn)
end
