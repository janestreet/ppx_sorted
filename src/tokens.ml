open Ppx_helpers.Base_lite
open Ppxlib

(* Tokens are stored in an immutable array sorted by source position (which is how
   [Ocaml_parser_lexer.file_tokens_exn] produces them), plus a [(start, stop)] window into
   that array. All operations run in [O(log N)] via binary search, and produce output
   whose size is proportional to the window, not the full file.

   Because tokens are non-overlapping and sorted by [loc_start.pos_cnum], they are also
   sorted (non-strictly) by [loc_end.pos_cnum]. That lets [limit] cut a contiguous range
   using two binary searches. *)
type t =
  { all_tokens : (Ocaml_common.Parser.token * Location.t) iarray
  ; start : int (* inclusive *)
  ; stop : int (* exclusive *)
  }

let create file_name ~contents =
  let all_tokens =
    Ocaml_parser_lexer.file_tokens_exn ~file_name ~file_contents:contents
    |> Iarray.of_list
  in
  { all_tokens; start = 0; stop = Iarray.length all_tokens }
;;

(* [lower_bound t ~pred] returns the smallest index [i] in [[t.start, t.stop]] such that
   [pred t.array.:(i)] holds. Requires [pred] to be monotone on [[t.start, t.stop)]
   (becomes true and stays true). Returns [t.stop] if no index satisfies [pred]. *)
let lower_bound t ~pred =
  let rec loop lo hi =
    if lo >= hi
    then lo
    else (
      let mid = lo + ((hi - lo) / 2) in
      if pred t.all_tokens.:(mid) then loop lo mid else loop (mid + 1) hi)
  in
  loop t.start t.stop
;;

let limit t (bounds : Location.t) =
  let start =
    lower_bound t ~pred:(fun (_, loc) ->
      loc.loc_start.pos_cnum >= bounds.loc_start.pos_cnum)
  in
  let stop =
    lower_bound { t with start } ~pred:(fun (_, loc) ->
      loc.loc_end.pos_cnum > bounds.loc_end.pos_cnum)
  in
  { t with start; stop }
;;

let forward t ~from =
  let i = lower_bound t ~pred:(fun (_, loc) -> loc.loc_start.pos_cnum >= from) in
  List.init (t.stop - i) ~f:(fun j -> t.all_tokens.:(i + j))
;;

let backward t ~from =
  (* First index whose [loc_end] is strictly past [from]; everything before that is
     eligible, and we return it in reverse order. *)
  let i_end = lower_bound t ~pred:(fun (_, loc) -> loc.loc_end.pos_cnum > from) in
  List.init (i_end - t.start) ~f:(fun j -> t.all_tokens.:(i_end - 1 - j))
;;
