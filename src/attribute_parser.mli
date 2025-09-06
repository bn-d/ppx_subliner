val to_bool : (Ppxlib.location * Ppxlib.structure) option -> bool
val to_expr : string -> Ppxlib.location * Ppxlib.structure -> Ppxlib.expression

val to_expr_opt :
  string ->
  (Ppxlib.location * Ppxlib.structure) option ->
  Ppxlib.expression option

val to_trimmed_string_expr_opt :
  string ->
  (Ppxlib.location * Ppxlib.structure) option ->
  Ppxlib.expression option

module Term : sig
  type 'a t = {
    (* term *)
    term : 'a option;
    (* info *)
    deprecated : 'a option;
    absent : 'a option;
    docs : 'a option;
    docv : 'a option;
    doc : 'a option;
    env : 'a option;
    env_deprecated : 'a option;
    env_docs : 'a option;
    env_doc : 'a option;
    (* named *)
    names : 'a option;
    opt_all : 'a option;
    (* positional *)
    pos : 'a option;
    pos_all : 'a option;
    pos_left : 'a option;
    pos_right : 'a option;
    rev : 'a option;
    (* as term *)
    non_empty : 'a option;
    last : 'a option;
    last_sep : 'a option;
    (* type *)
    default : 'a option;
  }
  [@@deriving make]

  val empty : 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) t
  (** parse attribute list to a static type *)
end

module Conv : sig
  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) option
  (** parse attribute list to a static type *)
end

module String_conv : sig
  type 'a t = { file : 'a option; dir : 'a option; non_dir_file : 'a option }
  [@@deriving make]

  val empty : 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) t
  (** parse attribute list to a static type *)
end

module Sep_conv : sig
  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) option
  (** parse attribute list to a static type *)
end

module Cmd_info : sig
  type 'a t = {
    deprecated : 'a option;
    man_xrefs : 'a option;
    man : 'a option;
    envs : 'a option;
    exits : 'a option;
    sdocs : 'a option;
    docs : 'a option;
    doc : 'a option;
    version : 'a option;
    name : 'a option;
  }
  [@@deriving make]

  val empty : 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t

  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) t
  (** parse attribute list to a static type *)
end

module Enum : sig
  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) option
  (** parse attribute list to a static type *)
end

module Default_term : sig
  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) option
  (** parse attribute list to a static type *)
end
