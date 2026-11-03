open Ppx_helpers.Base_lite
open Ppxlib

module Chunk = struct
  type t = string (* A run of all digits or all non-digits *)

  let is_digit c = Char.(c >= '0' && c <= '9')

  let split_string s =
    match String.length s with
    | 0 -> []
    | len ->
      let rec loop acc start i =
        if i >= len
        then List.rev (String.sub s ~pos:start ~len:(i - start) :: acc)
        else if Bool.( <> ) (is_digit s.[i]) (is_digit s.[start])
        then loop (String.sub s ~pos:start ~len:(i - start) :: acc) i (i + 1)
        else loop acc start (i + 1)
      in
      loop [] 0 1
  ;;

  (* natsort *)
  let compare a b =
    let is_digit_chunk s = String.length s > 0 && is_digit s.[0] in
    match is_digit_chunk a, is_digit_chunk b with
    | true, false -> -1 (* digits before non-digits *)
    | false, true -> 1
    | false, false -> String.compare a b (* non-digits lexicographically *)
    | true, true ->
      (* digits by numerical value *)
      (match Int.compare (String.length a) (String.length b) with
       | 0 -> String.compare a b
       | cmp -> cmp)
  ;;
end

type t =
  | Atom of Chunk.t list
  | List of t list

let rec sexp_of_t : t -> Sexp.t = function
  | Atom chunks -> Atom (String.concat ~sep:"" chunks)
  | List ts -> List (List.map ts ~f:sexp_of_t)
;;

let atom s = Atom (Chunk.split_string s)
let list xs = List xs

let rec compare a b =
  match a, b with
  | Atom _, List _ -> -1
  | List _, Atom _ -> 1
  | Atom a, Atom b -> List.compare ~cmp:Chunk.compare a b
  | List xs, List ys -> List.compare ~cmp:compare xs ys
;;

let flatten_list pexp_desc =
  let rec flatten acc = function
    | Pexp_construct
        ( { txt = Lident "::"; loc = _ }
        , Some { pexp_desc = Pexp_tuple [ (_, hd); (_, tl) ]; _ } ) ->
      flatten (hd :: acc) tl.pexp_desc
    | Pexp_construct ({ txt = Lident "[]"; loc = _ }, None) -> Some (List.rev acc)
    | _ -> None
  in
  flatten [] pexp_desc
;;

class of_ =
  object (self)
    inherit [t] Ast.lift as super

    method array : 'a. ('a -> t) -> 'a array -> t =
      fun f a -> list (List.map ~f (Array.to_list a))

    method bool b = atom (Bool.to_string b)
    method char c = atom (String.make 1 c)
    method constr name args = list (atom name :: args)
    method float f = atom (Float.to_string f)
    method int i = atom (Int.to_string i)
    method int32 i = atom (Int32.to_string i)
    method int64 i = atom (Int64.to_string i)
    method list : 'a. ('a -> t) -> 'a list -> t = fun f l -> list (List.map l ~f)
    method nativeint i = atom (Nativeint.to_string i)
    method other : 'a. 'a -> t = fun _ -> atom "_"

    method option f =
      function
      | Some a -> list [ f a ]
      | None -> list []

    method record fields = list (List.map fields ~f:(fun (a, _) -> atom a))
    method string = atom
    method tuple = list
    method unit () = list []

    method! structure_item { pstr_desc; pstr_loc = _ } =
      self#structure_item_desc pstr_desc

    method! value_binding { pvb_pat; _ } = self#pattern pvb_pat
    method! pattern { ppat_desc; _ } = self#pattern_desc ppat_desc

    method! pattern_desc ppat_desc =
      match ppat_desc with
      | Ppat_or (a, _) -> self#pattern a
      | ppat_desc -> super#pattern_desc ppat_desc

    method! loc : 'a. ('a -> t) -> 'a loc -> t = fun f { txt; loc = _ } -> f txt

    method! signature_item { psig_desc; psig_loc = _ } =
      self#signature_item_desc psig_desc

    method! value_description { pval_name; _ } = self#loc self#string pval_name
    method! constructor_declaration { pcd_name; _ } = self#loc self#string pcd_name
    method! label_declaration { pld_name; _ } = self#loc self#string pld_name

    method! row_field { prf_desc; _ } =
      match prf_desc with
      | Rtag (label, _, _) -> list [ atom "Rtag"; self#loc self#string label ]
      | Rinherit ct -> list [ atom "Rinherit"; self#core_type_desc ct.ptyp_desc ]

    method! module_binding { pmb_name; _ } = self#loc (self#option self#string) pmb_name

    method! module_declaration { pmd_name; _ } =
      self#loc (self#option self#string) pmd_name

    method! module_type_declaration { pmtd_name; _ } = self#loc self#string pmtd_name

    method! module_expr me =
      let rec flatten_to_longidents me =
        match me.pmod_desc with
        | Pmod_ident { txt; _ } -> [ self#longident txt ]
        | Pmod_apply (func, arg) -> flatten_to_longidents func @ flatten_to_longidents arg
        | _ -> [ self#module_expr_desc me.pmod_desc ]
      in
      list (flatten_to_longidents me)

    method! expression { pexp_desc; _ } = self#expression_desc pexp_desc

    method! expression_desc pexp_desc =
      match pexp_desc with
      | Pexp_construct ({ txt = Lident "::"; loc = _ }, _) ->
        (match flatten_list pexp_desc with
         | Some elts -> list (List.map elts ~f:self#expression)
         | None -> super#expression_desc pexp_desc)
      | pexp_desc -> super#expression_desc pexp_desc

    method! case { pc_lhs; _ } = self#pattern pc_lhs

    method! longident lid =
      let rec flatten acc = function
        | Lident s -> atom s :: acc
        | Ldot (prefix, s) -> flatten (atom s :: acc) prefix
        | Lapply (a, b) -> [ self#longident a; self#longident b ]
      in
      list (flatten [] lid)
  end

let of_ = new of_

let of_deriver expr =
  let rec name_of_expression expr =
    match expr.pexp_desc with
    | Pexp_ident { txt; _ } -> Some (Longident.name txt)
    | Pexp_apply (func, _) -> name_of_expression func
    | _ -> None
  in
  match name_of_expression expr with
  | Some name -> of_#string name
  | None -> of_#expression expr
;;

(* Resolve [None] keys by propagating from neighbors. Each [None]-keyed item gets the key
   of the first following [Some]-keyed item, or failing that, the first preceding
   [Some]-keyed item. Returns [None] if no items have keys.

   Each item also carries an [is_fixed] flag. When a [None]-keyed item inherits a key from
   a neighbor, it also inherits the neighbor's [is_fixed] flag, so that items like doc
   comments stay attached to the fixed item they belong to. *)
let resolve_none_keys with_keys =
  let carry_forward items =
    List.fold_map
      items
      ~init:(None, false)
      ~f:(fun (prev_key, prev_fixed) (key, fixed, item) ->
        let carried_key = Option.first_some key prev_key in
        let carried_fixed = if Option.is_some key then fixed else prev_fixed in
        (carried_key, carried_fixed), (carried_key, carried_fixed, item))
    |> snd
  in
  let carried_backward = List.rev with_keys |> carry_forward |> List.rev in
  let carried_forward = carry_forward carried_backward in
  let still_missing_keys, with_keys =
    List.partition_map carried_forward ~f:(function
      | None, _, item -> Left item
      | Some key, is_fixed, item -> Right (key, is_fixed, item))
  in
  match still_missing_keys with
  | [] -> Some with_keys
  | _ :: _ -> None
;;

let sort_if_necessary items ~f ~is_fixed : _ Nonempty_list.t option =
  (* [is_fixed] can have side effects (marking attributes as handled), so we call it once
     per item upfront. *)
  let with_keys = List.map items ~f:(fun item -> f item, is_fixed item, item) in
  match resolve_none_keys with_keys with
  | None -> None
  | Some prev ->
    let compare = Comparable.lift compare ~f:(fun (key, _, _) -> key) in
    let next =
      let fixed_indices, movable_items =
        List.partition_mapi prev ~f:(fun i ((_, is_fixed, _) as tuple) ->
          if is_fixed then Left i else Right tuple)
      in
      let fixed_indices = Int.Set.of_list fixed_indices in
      let sorted_movable_items =
        List.stable_sort movable_items ~cmp:compare |> Queue.of_list
      in
      List.mapi prev ~f:(fun i x ->
        if Int.Set.mem fixed_indices i then x else Queue.dequeue_exn sorted_movable_items)
    in
    (match List.compare ~cmp:compare prev next with
     | 0 -> None
     | _ -> Nonempty_list.of_list (List.map next ~f:(fun (_, _, item) -> item)))
;;
