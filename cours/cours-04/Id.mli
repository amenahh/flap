type t = string

val compare : t -> t -> int

val pp : t Fmt.t

val fresh : unit -> t

type 'a subst = (t * 'a) list

val pp_subst : 'a Fmt.t -> 'a subst Fmt.t

val empty : 'a subst

val extend : t -> 'a -> 'a subst -> 'a subst

val assoc : t -> 'a subst -> 'a

val singleton : t -> 'a -> 'a subst

val domain : 'a subst -> t list

type 'a rel = 'a -> 'a -> bool

val included : 'a rel -> 'a subst rel

val equal_subst : 'a rel -> 'a subst rel

val reset : unit -> unit

module Set : Set.S with type elt = t
