open Ppx_helpers.Base_lite
open Ppxlib

class of_ =
  let widen pos take_left a b =
    let a, b = pos a, pos b in
    match take_left a.pos_cnum b.pos_cnum with
    | true -> a
    | false -> b
  in
  let widen_start = widen (fun loc -> loc.loc_start) (fun a b -> a <= b) in
  let widen_end = widen (fun loc -> loc.loc_end) (fun a b -> a >= b) in
  let is_none (loc : location) =
    (* I don't know who's putting these into my AST but mixing the real locations with
       these none locations makes the ppx driver give up on lining up our corrections with
       the original source. Maybe these are from other ppxes? *)
    let is_none pos = String.equal pos.pos_fname Location.none.loc_start.pos_fname in
    is_none loc.loc_start || is_none loc.loc_end
  in
  object
    inherit [Location.t] Ast_traverse.fold

    method! location next prev =
      match is_none next, is_none prev with
      | true, false -> prev
      | false, true -> next
      | true, true -> prev (* might as well *)
      | false, false ->
        { prev with loc_start = widen_start prev next; loc_end = widen_end prev next }
  end

let of_ = new of_
let of_pattern pat = of_#pattern pat pat.ppat_loc
let of_expression expr = of_#expression expr expr.pexp_loc
let of_constructor_declaration c = of_#constructor_declaration c c.pcd_loc
let of_row_field rf = of_#row_field rf rf.prf_loc
let of_signature_item item = of_#signature_item item item.psig_loc

let of_label_declaration ld ~tokens =
  (* [pld_loc] for non-last record fields includes the trailing semicolon (e.g.,
     [zebra : int\n  ;]), which is problematic for sorting because each field carries its
     separator. Search backward from [pld_loc.loc_end]; if the first token is a SEMI, drop
     it. This preserves any field attributes (e.g., [[@sexp.bool]]) that appear between
     the type and the semicolon. *)
  let loc_end =
    match Tokens.backward tokens ~from:ld.pld_loc.loc_end.pos_cnum with
    | (SEMI, semi_loc) :: _ -> semi_loc.loc_start
    | _ -> ld.pld_loc.loc_end
  in
  { ld.pld_loc with loc_end }
;;

let of_case (c : case) ~tokens =
  let base_loc =
    Nonempty_list.reduce
      ~f:of_#location
      (of_#pattern c.pc_lhs c.pc_lhs.ppat_loc
       :: of_#expression c.pc_rhs c.pc_rhs.pexp_loc
       ::
       (match c.pc_guard with
        | None -> []
        | Some g -> [ of_#expression g g.pexp_loc ]))
  in
  (* Include the leading BAR if present, so that the "|" moves with the case. *)
  match Tokens.backward tokens ~from:base_loc.loc_start.pos_cnum with
  | (BAR, bar_loc) :: _ -> { base_loc with loc_start = bar_loc.loc_start }
  | _ -> base_loc
;;

let of_structure_item item = of_#structure_item item item.pstr_loc

let of_value_binding vb =
  (* The naive [of_#value_binding] would include the leading [let] and [and], which we
     don't want to carry along when we re-order the bindings. *)
  Nonempty_list.reduce
    ~f:of_#location
    (of_#pattern vb.pvb_pat vb.pvb_pat.ppat_loc
     :: of_#expression vb.pvb_expr vb.pvb_expr.pexp_loc
     :: List.map ~f:(fun attr -> of_#attribute attr attr.attr_loc) vb.pvb_attributes)
;;
