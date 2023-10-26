val list_of_string :
  string -> char list
val string_of_list :
  char list -> string

type 'a expr =
  | Eps
  | Base of 'a
  | Joker
  | Concat of 'a expr * 'a expr
  | Alt of 'a expr * 'a expr
  | Star of 'a expr

val pp_expr :
  Format.formatter -> char expr -> unit
val expr_to_string :
  char expr -> string

val sort_uniq :
  'a list -> 'a list
val union_sorted :
  'a list -> 'a list -> 'a list
