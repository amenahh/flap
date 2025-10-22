include Stdlib.Seq

let hd xs = uncons xs |> Option.map fst
let tl xs = uncons xs |> Option.map snd

let rec const x () = cons x (const x) ()

let rec nth : type a. int -> a t -> a option =
  fun n xs ->
  if n = 0 then hd xs else tl xs |> Option.map (nth (n - 1)) |> Option.join

let join : type a. a t t -> a t =
  let rec aux todo n k xss =
    let continue () = aux [] (n + 1) n (append List.(to_seq @@ rev todo) xss) in
    if k < 0 then continue ()
    else
      begin match uncons xss, todo with
      | None, [] ->
         empty
      | None, _ :: _ ->
         continue ()
      | Some (xs, xss), _ ->
         begin match uncons xs with
         | None -> aux todo n (k - 1) xss
         | Some (x, xs) -> fun () -> cons x (aux (xs :: todo) n (k - 1) xss) ()
         end
      end
  in
  fun xss () -> aux [] 1 0 xss ()

let ( let* ) xs f = map f xs |> concat

let rec combine f xs ys =
  match uncons xs, uncons ys with
  | Some (x, xs), Some (y, ys) -> fun () -> cons (f x y) (combine f xs ys) ()
  | None, _ | _, None -> empty

let rec dedup eq xs =
  match uncons xs with
  | None -> xs
  | Some (x, xs) -> cons x (filter (fun x' -> not (eq x x')) (dedup eq xs))

let rec nat : int t = fun () -> cons 0 (map ((+) 1) nat) ()

let nats : (int * int) t t =
  combine (fun x ys -> map (fun y -> (x, y)) ys) nat (const nat)
