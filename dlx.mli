module type DLXElement = sig
  type t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val compare : t -> t -> int
  val null : t
end

exception InitError
exception WrongTop of int
exception WrongLen of int

module type S = sig
  type elt
  type t

  val debug: bool ref

  val create: elt array -> elt array array -> t

  val pp_table: Format.formatter -> t -> unit

  val search: t -> unit
end

module DLX (E: DLXElement) : S with type elt = E.t
