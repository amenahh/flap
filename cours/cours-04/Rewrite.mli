module type LANG = sig
  type t
  val pp : t Fmt.t
  val equal : t -> t -> bool
  val rewrite : t -> t Seq.t
end

module Make(L : LANG) : sig
  type graph = (L.t * L.t) Seq.t
  val reductions : L.t -> graph
  val is_normal : L.t -> bool
  val normal_forms : L.t -> L.t Seq.t
  val display : ?verbose:bool -> ?max:int -> graph -> unit
end
