open Ppxlib
open! Ppx_helpers.Base_lite

type 'a t = 'a * Location.t

(** Backward widening: allow comments and whitespace on preceding lines. Stop at any
    non-comment, non-whitespace token. *)
let too_far_backward : Ocaml_common.Parser.token -> bool = function
  | COMMENT _ | DOCSTRING _ | EOL -> false
  | _ -> true
;;

(** Forward widening: allow comments on the same line only. *)
let too_far_forward : Ocaml_common.Parser.token -> bool = function
  | COMMENT _ | DOCSTRING _ -> false
  | _ -> true
;;

let rec widen tokens ~init ~get_pos ~is_delimiter ~too_far =
  match tokens with
  | [] -> init
  | (token, loc) :: rest ->
    if is_delimiter token || too_far token
    then init
    else widen rest ~init:(get_pos loc) ~get_pos ~is_delimiter ~too_far
;;

let make_loc ~start ~end_ = { loc_start = start; loc_end = end_; loc_ghost = false }

let create items tokens ~delimiter ~span_of =
  let is_delimiter =
    match delimiter with
    | None -> Fn.const false
    | Some d -> Poly.equal d
  in
  let widen_backward span prev_end =
    prev_end
    |> Option.fold ~init:tokens ~f:(fun tokens start ->
      Tokens.limit tokens (make_loc ~start ~end_:span.loc_end))
    |> Tokens.backward ~from:span.loc_start.pos_cnum
    |> widen
         ~init:span.loc_start
         ~get_pos:(fun loc -> loc.loc_start)
         ~is_delimiter
         ~too_far:too_far_backward
  in
  let widen_forward span next_start =
    next_start
    |> Option.fold ~init:tokens ~f:(fun tokens end_ ->
      Tokens.limit tokens (make_loc ~start:span.loc_start ~end_))
    |> Tokens.forward ~from:span.loc_end.pos_cnum
    |> widen
         ~init:span.loc_end
         ~get_pos:(fun loc -> loc.loc_end)
         ~is_delimiter
         ~too_far:too_far_forward
  in
  let rec process ~prev_end remaining =
    match remaining with
    | [] -> []
    | (item, span) :: rest ->
      let loc_start = widen_backward span prev_end in
      let loc_end =
        let next_start =
          List.hd rest |> Option.map ~f:(fun (_, next) -> next.loc_start)
        in
        widen_forward span next_start
      in
      let widened_loc = { span with loc_start; loc_end } in
      (item, widened_loc) :: process ~prev_end:(Some loc_end) rest
  in
  process ~prev_end:None (List.map items ~f:(fun item -> item, span_of item))
;;

let flatten_expression_elements pexp_desc =
  match Sort_key.flatten_list pexp_desc with
  | Some _ as result -> result
  | None ->
    (match pexp_desc with
     | Pexp_array (Immutable, elts) -> Some elts
     | _ -> None)
;;

let extract_value_bindings (expr : Ppxlib.expression) =
  match expr.pexp_desc with
  | Pexp_let (_, _, vbs, _) -> Some vbs
  | Pexp_extension
      ( _
      , PStr
          [ { pstr_desc = Pstr_eval ({ pexp_desc = Pexp_let (_, _, vbs, _); _ }, _); _ } ]
      ) -> Some vbs
  | _ -> None
;;

module Type_declaration = struct
  type nonrec t =
    | Regular_variant of constructor_declaration t list
    | Polymorphic_variant of row_field t list
    | Record of label_declaration t list

  let separator = function
    | Regular_variant _ -> ""
    | Polymorphic_variant _ -> "|"
    | Record _ -> "; "
  ;;
end

let of_row_fields row_fields ~enclosing_loc tokens =
  Tokens.limit tokens enclosing_loc
  |> create row_fields ~delimiter:(Some BAR) ~span_of:Span.of_row_field
;;

let ptype_loc_with_end_of_line td tokens =
  let end_of_line =
    Tokens.forward tokens ~from:td.ptype_loc.loc_end.pos_cnum
    |> widen
         ~init:td.ptype_loc.loc_end
         ~get_pos:(fun loc -> loc.loc_end)
         ~is_delimiter:(fun _ -> false)
         ~too_far:too_far_forward
  in
  { td.ptype_loc with loc_end = end_of_line }
;;

let of_type_declaration td tokens =
  match td.ptype_kind with
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | Some { ptyp_desc = Ptyp_variant (row_fields, _, _); _ } ->
       let row_fields = of_row_fields row_fields ~enclosing_loc:td.ptype_loc tokens in
       Some (Polymorphic_variant row_fields : Type_declaration.t)
     | _ -> None)
  | Ptype_variant cds ->
    let cds =
      ptype_loc_with_end_of_line td tokens
      |> Tokens.limit tokens
      |> create cds ~delimiter:(Some BAR) ~span_of:Span.of_constructor_declaration
    in
    Some (Regular_variant cds : Type_declaration.t)
  | Ptype_record lds ->
    let lds =
      let tokens = ptype_loc_with_end_of_line td tokens |> Tokens.limit tokens in
      let span_of = Span.of_label_declaration ~tokens in
      create lds tokens ~delimiter:(Some SEMI) ~span_of
    in
    Some (Record lds : Type_declaration.t)
  | Ptype_open | Ptype_record_unboxed_product _ -> None
;;

let of_polymorphic_variant_core_type ct tokens =
  match ct.ptyp_desc with
  | Ptyp_variant (row_fields, _, _) ->
    Some (of_row_fields row_fields ~enclosing_loc:ct.ptyp_loc tokens)
  | _ -> None
;;

let of_signature_or_structure_items items tokens ~span_of =
  create items tokens ~delimiter:None ~span_of
;;

let of_deriving_items items tokens =
  create items tokens ~delimiter:(Some COMMA) ~span_of:Span.of_expression
;;

let of_value_bindings (first :: rest : _ Nonempty_list.t) tokens =
  let loc_start = first.pvb_pat.ppat_loc.loc_start in
  let max_loc a b = if a.pos_cnum > b.pos_cnum then a else b in
  create (first :: rest) tokens ~delimiter:(Some AND) ~span_of:Span.of_value_binding
  |> List.map ~f:(fun (vb, span) ->
    (* We don't want to move any leading attributes on the first binding, e.g.,
       {[
         let[@sorted] a = ()
         and b = ()
       ]} *)
    vb, { span with loc_start = max_loc span.loc_start loc_start })
;;

module Expression = struct
  type nonrec t =
    | List_or_array of expression t list
    | Let_bindings of value_binding t list
    | Match_cases of case t list

  let separator = function
    | List_or_array _ -> " ; "
    | Let_bindings _ -> " and "
    | Match_cases _ -> ""
  ;;
end

let extract_match_cases (expr : Ppxlib.expression) =
  match expr.pexp_desc with
  | Pexp_match (_, cases) | Pexp_function (_, _, Pfunction_cases (cases, _, _)) ->
    Some cases
  | _ -> None
;;

let of_match_cases cases tokens =
  create cases tokens ~delimiter:(Some BAR) ~span_of:(Span.of_case ~tokens)
;;

let of_expression expr tokens =
  let tokens = Tokens.limit tokens expr.pexp_loc in
  match flatten_expression_elements expr.pexp_desc with
  | Some items ->
    Some
      (Expression.List_or_array
         (create items tokens ~delimiter:(Some SEMI) ~span_of:Span.of_expression))
  | None ->
    (match extract_value_bindings expr with
     | Some [] ->
       Location.raise_errorf
         ~loc:expr.pexp_loc
         "BUG: How do value bindings have zero bindings?"
     | Some (first :: rest) ->
       Some (Expression.Let_bindings (of_value_bindings (first :: rest) tokens))
     | None ->
       (match extract_match_cases expr with
        | Some cases -> Some (Expression.Match_cases (of_match_cases cases tokens))
        | None -> None))
;;
