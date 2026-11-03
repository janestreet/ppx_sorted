(** Attach source-code regions (including comments) to sortable items.

    Given a list of items with known AST spans, widens each span to include surrounding
    tokens (such as comments) that "belong" to the item. *)

open Ppx_helpers.Base_lite
open Ppxlib

type 'a t = 'a * Location.t

(** [create items tokens ~delimiter ~span_of ] attaches to each [item] in [items] the
    widest location that satisfies the following conditions:
    - contains [span_of item]
    - does not cross anything but newlines and comments going backward
    - does not cross anything but comments going forward
    - does not contain [delimiter], if that is not [None]
    - does not overlap the next or previous items in [items]
    - does not contain incomplete tokens *)
val create
  :  'a list
  -> Tokens.t
  -> delimiter:Ocaml_common.Parser.token option
  -> span_of:('a -> Location.t)
  -> 'a t list

module Expression : sig
  type nonrec t =
    | List_or_array of expression t list
    | Let_bindings of value_binding t list
    | Match_cases of case t list

  val separator : t -> string
end

(** [of_expression expr tokens] attaches source regions to the sortable elements of an
    expression. Returns [Some (List_or_array ...)] for list and iarray literals,
    [Some (Let_bindings ...)] for [let ... and] expressions (including inside
    [let%ext ... and] extensions) with two or more bindings, or [None] otherwise. *)
val of_expression : expression -> Tokens.t -> Expression.t option

module Type_declaration : sig
  type nonrec t =
    | Regular_variant of constructor_declaration t list
    | Polymorphic_variant of row_field t list
    | Record of label_declaration t list

  (** Returns [""] for regular variants (whose [pcd_loc] already includes the BAR), ["|"]
      for polymorphic variants (whose [prf_loc] does not), and ["; "] for record fields
      (whose [pld_loc] includes the trailing semicolon). *)
  val separator : t -> string
end

(** [of_type_declaration td tokens] attaches source regions to the constructors, row
    fields, or label declarations of a type declaration (regular variant, polymorphic
    variant, or record). Returns [None] if [td] is none of these. *)
val of_type_declaration : type_declaration -> Tokens.t -> Type_declaration.t option

(** [of_polymorphic_variant_core_type ct tokens] is like [of_polymorphic_variant_type] but
    operates on a bare [core_type] (e.g., an anonymous polymorphic variant in an
    expression or signature annotation). Returns [None] if [ct] is not a [Ptyp_variant]. *)
val of_polymorphic_variant_core_type : core_type -> Tokens.t -> row_field t list option

(** [of_signature_or_structure_items items tokens ~span_of] attaches source regions to the
    signature or structure items. Behavior is undefined if ['a] is anything else. *)
val of_signature_or_structure_items
  :  'a list
  -> Tokens.t
  -> span_of:('a -> Location.t)
  -> 'a t list

(** [of_deriving_items exprs tokens] attaches source regions to the expressions in a
    [@@deriving ...] attribute payload. Uses COMMA as the delimiter. *)
val of_deriving_items : expression list -> Tokens.t -> expression t list

(** [of_value_bindings vbs tokens] attaches source regions to the individual
    [value_binding]s in a [let ... and ... and ...] construct, for use with top-level
    structure items ([Pstr_value]). *)
val of_value_bindings : value_binding Nonempty_list.t -> Tokens.t -> value_binding t list
