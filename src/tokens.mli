(** Token-level navigation over OCaml source code.

    Provides forward and backward token lists from a given position, backed by
    [Ocaml_parser_lexer.file_tokens_exn]. *)

open Ppxlib

type t

(** [create filename ~contents] tokenizes [contents] as file [filename]. *)
val create : string -> contents:string -> t

(** [limit t loc] drops all tokens outside [loc]. *)
val limit : t -> Location.t -> t

(** [forward t ~from] returns tokens whose start position ([pos_cnum]) is >= [from], in
    order of increasing position. *)
val forward : t -> from:int -> (Ocaml_common.Parser.token * Location.t) list

(** [backward t ~from] returns tokens whose end position ([pos_cnum]) is <= [from], in
    order of decreasing position (i.e., scanning leftward). *)
val backward : t -> from:int -> (Ocaml_common.Parser.token * Location.t) list
