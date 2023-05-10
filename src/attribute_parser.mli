module Term : sig
  type 'a t = {
    (* info *)
    deprecated : 'a option;
    absent : 'a option;
    doc : 'a option;
    docs : 'a option;
    docv : 'a option;
    env : 'a option;
    (* named *)
    names : 'a option;
    (* positional *)
    pos : 'a option;
    pos_all : 'a option;
    pos_left : 'a option;
    pos_right : 'a option;
    (* list *)
    non_empty : 'a option;
    last : 'a option;
  }

  val empty : 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val parse : Ppxlib.attributes -> (Ppxlib.location * Ppxlib.structure) t
end

module Cmd_info : sig
  val to_args :
    default_name_expr:Ppxlib.expression ->
    Ppxlib.attributes ->
    (Ppxlib.arg_label * Ppxlib.expression) list
end

module Default_term : sig
  val get : Ppxlib.attributes -> Ppxlib.expression option
end
