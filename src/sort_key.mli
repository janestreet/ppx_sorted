open Ppx_helpers.Base_lite
open Ppxlib

(** A tree-shaped sort key derived from the AST, used to determine sort order.

    Comparison uses a natural sort: [Atom]s (simple values) sort before [List]s (compound
    values), atom strings are split into digit/non-digit chunks and digit chunks are
    compared numerically, and [List]s are compared element-by-element. See the ppx_sorted
    guide for detailed examples. *)
type t

val compare : t -> t -> int
val sexp_of_t : t -> Sexp.t

(** [sort_if_necessary items ~f] returns [None] if [items] is already sorted by increasing
    [f], or the new sort order otherwise.

    If [f] returns [None] for an item, the item is not sorted independently. Instead, it
    sorts using the key of the first following item whose key is [Some], or failing that,
    the first preceding item whose key is [Some]. If no items have keys, returns [None].

    If [is_fixed] returns [true] for an item, that item's position in the list is not
    changed by sorting. The remaining (non-fixed) items are sorted around the fixed items. *)
val sort_if_necessary
  :  'a list
  -> f:('a -> t option)
  -> is_fixed:('a -> bool)
  -> 'a Nonempty_list.t option

class of_ : object
  inherit [t] Ppxlib_traverse_builtins.std_lifters
  inherit [t] Ast.lift
end

val of_ : of_

(** [of_deriver expr] extracts a sort key from a deriver expression (e.g., [sexp] or
    [sexp ~portable]). Sorts by the deriver name, falling back to the full expression AST
    for unusual cases. *)
val of_deriver : expression -> t

(** [flatten_list pexp_desc] returns the elements of the list if [pexp_desc] is a list. *)
val flatten_list : expression_desc -> expression list option
