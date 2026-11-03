open Ppxlib

class of_ : object
  inherit [Location.t] Ast_traverse.fold
end

val of_ : of_
val of_pattern : pattern -> Location.t
val of_case : case -> tokens:Tokens.t -> Location.t
val of_constructor_declaration : constructor_declaration -> Location.t
val of_expression : expression -> Location.t
val of_label_declaration : label_declaration -> tokens:Tokens.t -> Location.t
val of_row_field : row_field -> Location.t
val of_signature_item : signature_item -> Location.t
val of_structure_item : structure_item -> Location.t
val of_value_binding : value_binding -> Location.t
