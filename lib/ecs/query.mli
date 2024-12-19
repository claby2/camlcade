module Filter : sig
  type t =
    | With of Id.Component.t
    | Without of Id.Component.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Wildcard

  val matches : t -> Id.ComponentSet.t -> bool
end

type term = Required of Id.Component.t | Optional of Id.Component.t
type t

val create : ?filter:Filter.t -> term list -> t
val terms : t -> term list
val filter : t -> Filter.t

module Result : sig
  type t = (Id.Entity.t * Component.value list) list
end
