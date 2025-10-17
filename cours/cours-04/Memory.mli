module Address : sig
  type t
  val pp : Format.formatter -> t -> unit
end

type 'a t
val empty : 'a t
val alloc : 'a -> 'a t -> Address.t * 'a t
val read : Address.t -> 'a t -> 'a
val asgn : Address.t -> 'a -> 'a t -> 'a t
