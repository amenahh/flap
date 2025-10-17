module Address = struct
  type t = int
  let compare (x : int) y = Stdlib.compare x y
  let pp ff a = Format.fprintf ff "\@%d" a
end

module M = Map.Make(Address)
type 'a t = { free : int; contents : 'a M.t; }

let empty = { free = 0; contents = M.empty; }

let alloc v0 { free; contents; } =
  free, { free = free + 1; contents = M.add free v0 contents; }

let read a { contents; _ } = M.find a contents

let asgn a v { free; contents; } =
  if not (M.mem a contents) then invalid_arg "asgn: unknown addrses";
  { free; contents = M.add a v contents; }
