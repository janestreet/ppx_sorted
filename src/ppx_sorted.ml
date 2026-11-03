module Span = Span
module Tokens = Tokens
module With_source_code = With_source_code
open Ppx_helpers.Base_lite
open Ppxlib

module Driver : sig
  include module type of struct
    include Driver
  end

  val register_correction : loc:Location.t -> repl:string -> unit
  [@@alert use_corrections_module "Use Corrections module"]
end =
  Ppxlib.Driver

module Corrections : sig
  type t

  val create : unit -> t
  val is_empty : t -> bool
  val register : t -> loc:Location.t -> repl:string -> unit
end = struct
  type t = { mutable is_empty : bool }

  let create () = { is_empty = true }
  let is_empty t = t.is_empty

  let register t ~loc ~repl =
    t.is_empty <- false;
    (Driver.register_correction [@alert "-use_corrections_module"]) ~loc ~repl
  ;;
end

let attribute_name = "sorted"
let sorted_begin_attr = "sorted.begin"
let sorted_end_attr = "sorted.end"
let sorted_fixed_attr = "sorted.fixed"

let is_handled_attribute_name ~sort_deriving name =
  String.equal name attribute_name
  || String.equal name sorted_begin_attr
  || String.equal name sorted_end_attr
  || String.equal name sorted_fixed_attr
  || (sort_deriving && String.equal name "deriving")
;;

let find_attr name attrs =
  List.find attrs ~f:(fun attr -> String.equal attr.attr_name.txt name)
;;

let has_attr name attrs = Option.is_some (find_attr name attrs)

let mark_attr_as_handled name attrs =
  List.iter attrs ~f:(fun attr ->
    if String.equal attr.attr_name.txt name then Attribute.mark_as_handled_manually attr)
;;

let is_fixed_attr attrs =
  let is_fixed = has_attr sorted_fixed_attr attrs in
  if is_fixed then mark_attr_as_handled sorted_fixed_attr attrs;
  is_fixed
;;

let sort_deriving = ref false

let () =
  Driver.add_arg
    "-sort-deriving"
    (Bool (( := ) sort_deriving))
    ~doc:"BOOL Sort derivers in [@@deriving ...] attributes (default: false)"
;;

type t =
  { corrections : Corrections.t
  ; sort_deriving : bool
  ; tokens : Tokens.t
  ; contents : string
  }

let source_text t loc =
  String.sub
    t.contents
    ~pos:loc.loc_start.pos_cnum
    ~len:(loc.loc_end.pos_cnum - loc.loc_start.pos_cnum)
;;

let register_correction t items ~sep =
  let locations = Nonempty_list.map items ~f:snd in
  let loc = Nonempty_list.reduce locations ~f:Span.of_#location in
  let replacement =
    Nonempty_list.map locations ~f:(source_text t)
    |> Nonempty_list.to_list
    |> String.concat ~sep
  in
  Corrections.register t.corrections ~loc ~repl:replacement
;;

let sort_if_necessary t items ~sort_key ~sep ~is_fixed =
  Sort_key.sort_if_necessary
    items
    ~f:(fun (a, _) -> sort_key a)
    ~is_fixed:(fun (a, _) -> is_fixed a)
  |> Option.iter ~f:(register_correction t ~sep)
;;

let sort_if_necessary_some t items ~sort_key ~sep ~is_fixed =
  sort_if_necessary t items ~sort_key:(fun a -> Some (sort_key a)) ~sep ~is_fixed
;;

let sort_deriving_attribute t attr =
  match attr.attr_payload with
  | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] ->
    let exprs =
      match expr.pexp_desc with
      | Pexp_tuple elts -> List.map elts ~f:snd
      | _ -> [ expr ]
    in
    Tokens.limit t.tokens expr.pexp_loc
    |> With_source_code.of_deriving_items exprs
    |> sort_if_necessary_some
         t
         ~sort_key:Sort_key.of_deriver
         ~sep:", "
         ~is_fixed:(fun expr -> is_fixed_attr expr.pexp_attributes)
  | _ -> ()
;;

let sort_deriving_attributes t attrs =
  List.iter attrs ~f:(fun attr ->
    if String.equal attr.attr_name.txt "deriving" then sort_deriving_attribute t attr)
;;

let sort_type_declaration t td =
  mark_attr_as_handled attribute_name td.ptype_attributes;
  (* Skip correction for ghost-located types (generated code, e.g. ppx_packed's alt type).
     Registering a correction at a ghost location can overlap with the real type's
     correction and crash the PPX driver. *)
  if td.ptype_loc.loc_ghost
  then td
  else (
    let tokens = Tokens.limit t.tokens td.ptype_loc in
    (match With_source_code.of_type_declaration td tokens with
     | None ->
       Location.raise_errorf
         ~loc:td.ptype_loc
         "[@@sorted] can only be applied to variant or record types"
     | Some type_decl ->
       let sep = With_source_code.Type_declaration.separator type_decl in
       (match type_decl with
        | Regular_variant attached ->
          sort_if_necessary_some
            t
            attached
            ~sort_key:Sort_key.of_#constructor_declaration
            ~sep
            ~is_fixed:(fun cd -> is_fixed_attr cd.pcd_attributes)
        | Polymorphic_variant attached ->
          sort_if_necessary_some
            t
            attached
            ~sort_key:Sort_key.of_#row_field
            ~sep
            ~is_fixed:(fun rf -> is_fixed_attr rf.prf_attributes)
        | Record attached ->
          sort_if_necessary_some
            t
            attached
            ~sort_key:Sort_key.of_#label_declaration
            ~sep
            ~is_fixed:(fun ld -> is_fixed_attr ld.pld_attributes)));
    td)
;;

let make_process_items
  (type item)
  ~(span_of : item -> Location.t)
  ~(get_attr : item -> attribute option)
  ~(get_sort_key : item -> Sort_key.t option)
  ~(is_fixed : item -> bool)
  : t -> item list -> item list
  =
  let open struct
    let is_attr item attr_name =
      match get_attr item with
      | Some attr -> String.equal attr.attr_name.txt attr_name
      | None -> false
    ;;

    let mark_attr_as_handled item =
      match get_attr item with
      | Some attr -> Attribute.mark_as_handled_manually attr
      | None -> ()
    ;;

    let rec process_items t items =
      match items with
      | [] -> []
      | begin_item :: rest when is_attr begin_item sorted_begin_attr ->
        mark_attr_as_handled begin_item;
        let rec collect_until_end acc remaining =
          match remaining with
          | [] ->
            Location.raise_errorf
              ~loc:(span_of begin_item)
              "[@@@sorted.begin] without matching [@@@sorted.end]"
          | end_item :: rest when is_attr end_item sorted_end_attr ->
            mark_attr_as_handled end_item;
            List.rev acc, end_item, rest
          | x :: rest -> collect_until_end (x :: acc) rest
        in
        let items_to_sort, end_item, remaining = collect_until_end [] rest in
        { loc_start = (span_of begin_item).loc_end
        ; loc_end = (span_of end_item).loc_start
        ; loc_ghost = false
        }
        |> Tokens.limit t.tokens
        |> With_source_code.of_signature_or_structure_items items_to_sort ~span_of
        |> sort_if_necessary t ~sort_key:get_sort_key ~sep:"\n" ~is_fixed;
        let remaining = process_items t remaining in
        List.concat [ [ begin_item ]; items_to_sort; [ end_item ]; remaining ]
      | item :: _rest when is_attr item sorted_end_attr ->
        Location.raise_errorf
          ~loc:(span_of item)
          "[@@@sorted.end] without matching [@@@sorted.begin]"
      | item :: rest -> item :: process_items t rest
    ;;
  end in
  process_items
;;

let is_floating_doc_attribute attr =
  String.equal attr.attr_name.txt "ocaml.doc"
  || String.equal attr.attr_name.txt "ocaml.text"
;;

let structure_item_is_fixed (si : structure_item) =
  match si.pstr_desc with
  | Pstr_value (_, vbs) ->
    (match vbs with
     | [ vb ] -> is_fixed_attr vb.pvb_attributes
     | _ -> false)
  | Pstr_primitive vd -> is_fixed_attr vd.pval_attributes
  | Pstr_type (_, tds) ->
    (match tds with
     | [ td ] -> is_fixed_attr td.ptype_attributes
     | _ -> false)
  | Pstr_module mb -> is_fixed_attr mb.pmb_attributes
  | Pstr_modtype mtd -> is_fixed_attr mtd.pmtd_attributes
  | Pstr_open od -> is_fixed_attr od.popen_attributes
  | Pstr_include id -> is_fixed_attr id.pincl_attributes
  | Pstr_exception te -> is_fixed_attr te.ptyexn_attributes
  | Pstr_eval (_, attrs) -> is_fixed_attr attrs
  | Pstr_extension (_, attrs) -> is_fixed_attr attrs
  | Pstr_typext te -> is_fixed_attr te.ptyext_attributes
  | Pstr_recmodule _ | Pstr_class _ | Pstr_class_type _ | Pstr_attribute _ | Pstr_jkind _
    -> false
;;

let signature_item_is_fixed (si : signature_item) =
  match si.psig_desc with
  | Psig_value vd -> is_fixed_attr vd.pval_attributes
  | Psig_type (_, tds) ->
    (match tds with
     | [ td ] -> is_fixed_attr td.ptype_attributes
     | _ -> false)
  | Psig_module md -> is_fixed_attr md.pmd_attributes
  | Psig_modtype mtd -> is_fixed_attr mtd.pmtd_attributes
  | Psig_open od -> is_fixed_attr od.popen_attributes
  | Psig_include (id, _) -> is_fixed_attr id.pincl_attributes
  | Psig_exception te -> is_fixed_attr te.ptyexn_attributes
  | Psig_extension (_, attrs) -> is_fixed_attr attrs
  | Psig_typext te -> is_fixed_attr te.ptyext_attributes
  | Psig_modsubst ms -> is_fixed_attr ms.pms_attributes
  | Psig_modtypesubst mtd -> is_fixed_attr mtd.pmtd_attributes
  | Psig_typesubst tds ->
    (match tds with
     | [ td ] -> is_fixed_attr td.ptype_attributes
     | _ -> false)
  | Psig_recmodule _ | Psig_class _ | Psig_class_type _ | Psig_attribute _ | Psig_jkind _
    -> false
;;

let process_structure_items =
  make_process_items
    ~span_of:Span.of_structure_item
    ~get_attr:(fun item ->
      match item.pstr_desc with
      | Pstr_attribute attr -> Some attr
      | _ -> None)
    ~get_sort_key:(fun item ->
      match item.pstr_desc with
      | Pstr_attribute attr when is_floating_doc_attribute attr -> None
      | _ -> Some (Sort_key.of_#structure_item item))
    ~is_fixed:structure_item_is_fixed
;;

let process_signature_items =
  make_process_items
    ~span_of:Span.of_signature_item
    ~get_attr:(fun item ->
      match item.psig_desc with
      | Psig_attribute attr -> Some attr
      | _ -> None)
    ~get_sort_key:(fun item ->
      match item.psig_desc with
      | Psig_attribute attr when is_floating_doc_attribute attr -> None
      | _ -> Some (Sort_key.of_#signature_item item))
    ~is_fixed:signature_item_is_fixed
;;

let has_sorted_on_value_bindings vbs =
  List.exists vbs ~f:(fun vb -> has_attr attribute_name vb.pvb_attributes)
;;

let mark_sorted_on_value_bindings_as_handled vbs =
  List.iter vbs ~f:(fun vb -> mark_attr_as_handled attribute_name vb.pvb_attributes)
;;

class sort_in =
  object (self)
    inherit [t] Ast_traverse.map_with_context as super

    method private sort_subexpressions t elements =
      let t = { t with corrections = Corrections.create () } in
      List.iter elements ~f:(fun (elt, _) -> ignore (self#expression t elt : expression));
      match Corrections.is_empty t.corrections with
      | true -> `Did_not_sort
      | false -> `Did_sort

    method! pattern t pat =
      let pat = super#pattern t pat in
      (match has_attr attribute_name pat.ppat_attributes with
       | false -> ()
       | true ->
         mark_attr_as_handled attribute_name pat.ppat_attributes;
         let tokens = Tokens.limit t.tokens pat.ppat_loc in
         (match pat.ppat_desc with
          | Ppat_or _ ->
            let alternatives =
              let rec collect acc pat =
                match pat.ppat_desc with
                | Ppat_or (lhs, rhs) -> collect (rhs :: acc) lhs
                | _ -> pat :: acc
              in
              collect [] pat
            in
            With_source_code.create
              alternatives
              tokens
              ~delimiter:(Some BAR)
              ~span_of:Span.of_pattern
            |> sort_if_necessary_some
                 t
                 ~sort_key:Sort_key.of_#pattern
                 ~sep:" | "
                 ~is_fixed:(fun p -> is_fixed_attr p.ppat_attributes)
          | _ ->
            Location.raise_errorf
              ~loc:pat.ppat_loc
              "[@sorted] can only be applied to or-patterns when used on patterns"));
      pat

    method! expression t expr =
      let expr = super#expression t expr in
      let tokens = Tokens.limit t.tokens expr.pexp_loc in
      let sorted_attrs =
        let expr_has_sorted_attr = find_attr attribute_name expr.pexp_attributes in
        let function_cases_has_sorted_attr =
          match expr.pexp_desc with
          | Pexp_function (_, _, Pfunction_cases (_, _, attrs)) ->
            find_attr attribute_name attrs
          | _ -> None
        in
        List.filter_opt [ expr_has_sorted_attr; function_cases_has_sorted_attr ]
      in
      let has_sorted_attr = not (List.is_empty sorted_attrs) in
      (match With_source_code.of_expression expr tokens with
       | None ->
         if has_sorted_attr
         then
           Location.raise_errorf
             ~loc:expr.pexp_loc
             "[@sorted] can only be applied to list literals, iarray literals, let...and \
              bindings, or match expressions"
       | Some with_source_code_expr ->
         let sep = With_source_code.Expression.separator with_source_code_expr in
         List.iter sorted_attrs ~f:Attribute.mark_as_handled_manually;
         (match with_source_code_expr with
          | List_or_array elements ->
            if has_sorted_attr
            then (
              match self#sort_subexpressions t elements with
              | `Did_sort -> ()
              | `Did_not_sort ->
                sort_if_necessary_some
                  t
                  elements
                  ~sort_key:Sort_key.of_#expression
                  ~sep
                  ~is_fixed:(fun expr -> is_fixed_attr expr.pexp_attributes))
          | Let_bindings attached ->
            let vbs_has_sorted_attr =
              let vbs = List.map attached ~f:fst in
              let vbs_has_sorted_attr = has_sorted_on_value_bindings vbs in
              if vbs_has_sorted_attr then mark_sorted_on_value_bindings_as_handled vbs;
              vbs_has_sorted_attr
            in
            if has_sorted_attr || vbs_has_sorted_attr
            then
              sort_if_necessary_some
                t
                attached
                ~sort_key:Sort_key.of_#value_binding
                ~sep
                ~is_fixed:(fun vb -> is_fixed_attr vb.pvb_attributes)
          | Match_cases attached ->
            if has_sorted_attr
            then
              sort_if_necessary_some
                t
                attached
                ~sort_key:Sort_key.of_#case
                ~sep
                ~is_fixed:(fun (c : case) -> is_fixed_attr c.pc_lhs.ppat_attributes)));
      expr

    method! structure t structure =
      process_structure_items t (super#structure t structure)

    method! signature t signature =
      let signature = super#signature t signature in
      { signature with psg_items = process_signature_items t signature.psg_items }

    method! structure_item t si =
      let si = super#structure_item t si in
      (match si.pstr_desc with
       | Pstr_value (_, (first :: (_ :: _ as rest) as vbs)) ->
         (match has_sorted_on_value_bindings vbs with
          | false -> ()
          | true ->
            mark_sorted_on_value_bindings_as_handled vbs;
            Tokens.limit t.tokens si.pstr_loc
            |> With_source_code.of_value_bindings (first :: rest)
            |> sort_if_necessary_some
                 t
                 ~sort_key:Sort_key.of_#value_binding
                 ~sep:"\nand "
                 ~is_fixed:(fun vb -> is_fixed_attr vb.pvb_attributes))
       | _ -> ());
      si

    method! type_declaration t td =
      let td = super#type_declaration t td in
      if t.sort_deriving then sort_deriving_attributes t td.ptype_attributes;
      match has_attr attribute_name td.ptype_attributes with
      | false -> td
      | true -> sort_type_declaration t td

    method! core_type t ct =
      let ct = super#core_type t ct in
      (match has_attr attribute_name ct.ptyp_attributes with
       | false -> ()
       | true ->
         mark_attr_as_handled attribute_name ct.ptyp_attributes;
         Tokens.limit t.tokens ct.ptyp_loc
         |> With_source_code.of_polymorphic_variant_core_type ct
         |> (function
          | Some attached ->
            sort_if_necessary_some
              t
              attached
              ~sort_key:Sort_key.of_#row_field
              ~sep:"|"
              ~is_fixed:(fun rf -> is_fixed_attr rf.prf_attributes)
          | None ->
            Location.raise_errorf
              ~loc:ct.ptyp_loc
              "[@sorted] can only be applied to polymorphic variant types"));
      ct

    method! module_expr t me =
      let me = super#module_expr t me in
      match me.pmod_desc with
      | Pmod_structure structure ->
        let processed = process_structure_items t structure in
        { me with pmod_desc = Pmod_structure processed }
      | _ -> me

    method! module_type t mt =
      let mt = super#module_type t mt in
      match mt.pmty_desc with
      | Pmty_signature signature ->
        let processed_items = process_signature_items t signature.psg_items in
        { mt with
          pmty_desc = Pmty_signature { signature with psg_items = processed_items }
        }
      | _ -> mt
  end

let sort_in = new sort_in

let create filename =
  let contents = In_channel.read_all filename in
  let tokens = Tokens.create filename ~contents in
  { corrections = Corrections.create ()
  ; sort_deriving = !sort_deriving
  ; tokens
  ; contents
  }
;;

(* AST-level short-circuit: if structure doesn't contain [@sorted] attributes, skip
   processing it.

   Note that this is relatively safe: if there's a bug in this code and we accidentally
   skip processing a file which does contain attributes we handle, ppxlib will alert on
   unhandled attributes. *)
let contains_handled_attribute ~sort_deriving =
  object
    inherit [bool] Ast_traverse.fold as super

    method! attribute attr acc =
      match acc with
      | true -> acc
      | false ->
        (match is_handled_attribute_name ~sort_deriving attr.attr_name.txt with
         | true -> true
         | false -> super#attribute attr acc)
  end
;;

let structure_uses_ppx_sorted ~sort_deriving structure =
  (contains_handled_attribute ~sort_deriving)#structure structure false
;;

let signature_uses_ppx_sorted ~sort_deriving signature =
  (contains_handled_attribute ~sort_deriving)#signature signature false
;;

let () =
  Driver.register_transformation
    "sorted"
    (* [preprocess_impl] and [preprocess_intf] ensure that ppx_sort runs before ppx_let,
       so that we can sort [let%map_open.Command...and] before it gets expanded away. *)
    ~preprocess_impl:(fun structure ->
      match structure with
      | [] -> structure
      | first :: _ as structure ->
        (match structure_uses_ppx_sorted ~sort_deriving:!sort_deriving structure with
         | false -> structure
         | true ->
           let t = create first.pstr_loc.loc_start.pos_fname in
           sort_in#structure t structure))
    ~preprocess_intf:(fun signature ->
      match signature.psg_items with
      | [] -> signature
      | first :: _ ->
        (match signature_uses_ppx_sorted ~sort_deriving:!sort_deriving signature with
         | false -> signature
         | true ->
           let t = create first.psig_loc.loc_start.pos_fname in
           sort_in#signature t signature))
;;
