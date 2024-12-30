(** Defines components. *)

(** {1:components Components} *)

type base
(** Extensible variant type for components.

    This should only be used internally. *)

(** Signature for a component. *)
module type S = sig
  type t
  (** The internal data of the component. *)

  val id : Id.Component.t
  (** Unique identifier for the component. *)

  val of_base : base -> t
  (** Convert a {!Ecs.Component.base} to a component. *)

  val of_base_opt : base -> t option
  (** Convert a {!Ecs.Component.base} to a component, returning [None] if the
      base is not valid. *)

  val to_base : t -> base
  (** Convert a component to a {!Ecs.Component.base}. *)
end

(** Create a new component module. *)
module Make (B : sig
  type inner
end) : S with type t = B.inner

(** A component that doesn't have any data. Mainly used for optional components
    in a query result. *)
module None : sig
  type t = unit

  module C : S with type t = t
end

(** {1:packed Packed} *)

type packed
(** A type-erased component. *)

val pack : 'a. (module S with type t = 'a) -> 'a -> packed
(** [pack component value] packs the component [value] into a
    {!Ecs.Component.type-packed}. *)

val unpack : 'a. (module S with type t = 'a) -> packed -> 'a
(** [unpack component packed] unpacks the component from the
    {!Ecs.Component.type-packed}. *)

val unpack_opt : 'a. (module S with type t = 'a) -> packed -> 'a option
(** [unpack_opt component packed] unpacks the component from the
    {!Ecs.Component.type-packed}, returning [None] if the component is not
    valid. *)

val id : packed -> Id.Component.t
(** [id packed] returns the unique identifier of the component. *)
