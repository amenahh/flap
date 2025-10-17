module S = struct
  type t = string

  let compare (x : t) y = Stdlib.compare x y

  let pp = Fmt.string
end

include S

let fresh, reset =
  let r = ref 0 in
  (fun () -> incr r; Printf.sprintf "_x%d" !r),
  (fun () -> r := 0)

type 'a subst = (t * 'a) list

let pp_subst pp_val =
  let pp_kv ff (x, v) = Format.fprintf ff "%a\\%a" pp x pp_val v in
  Fmt.(box (list ~sep:comma pp_kv))

let empty = []

let extend x v xs = (x, v) :: xs

let assoc = List.assoc

let singleton x v = [(x, v)]

let trim xs =
  List.fold_right
    (fun (x, v) xs -> (x, v) :: List.remove_assoc x xs)
    xs
    []

let domain xs = trim xs |> List.map fst

type 'a rel = 'a -> 'a -> bool

let included r xs ys =
  try List.for_all (fun (x, v) -> r v (List.assoc x ys)) (trim xs)
  with Not_found -> false

let equal_subst eq xs ys = included eq xs ys && included eq ys xs

module Set = Set.Make(S)
