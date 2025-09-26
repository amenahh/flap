include Stdlib.List

let map_fold_right
        : type a b c. (a -> b -> c * b) -> a list -> b -> c list * b =
  fun f xs acc ->
  fold_right
    (fun x (ys, acc) -> let y, acc = f x acc in y :: ys, acc)
    xs
    ([], acc)
