(** PPX that asserts at compile time that:
    - A list literal is sorted (using [[@sorted]])
    - Variant type constructors are sorted alphabetically (using [[@@sorted]])
    - Polymorphic variant type constructors are sorted alphabetically (using [[@@sorted]]
      on type declarations or [[@sorted]] on anonymous polymorphic variant types)
    - Let/val bindings between [[@@@sorted.begin]] and [[@@@sorted.end]] are sorted
      alphabetically by name
    - Derivers in [[@@deriving ...]] attributes on type declarations are sorted
      alphabetically by deriver name
    - [let ... and] bindings are sorted by pattern name (using [[@sorted]] on the [let]
      expression, including [let%map_open.Command] and similar ppx let extensions)
    - Or-pattern alternatives are sorted (using [[@sorted]] on the or-pattern, e.g.,
      [(B | A)[@sorted]] becomes [(A | B)[@sorted]])

    Individual items can be pinned to their position using [[@sorted.fixed]] (or
    [[@@sorted.fixed]] for declarations). Fixed items are not moved; the remaining items
    are sorted around them.

    Works in both .ml (structure) and .mli (signature) files. See ../README.md for more
    information. *)

module Span = Span
module Tokens = Tokens
module With_source_code = With_source_code
