open Regex_base

val repeat :
  int -> 'a list -> 'a list
val expr_repeat :
  int -> 'a expr -> 'a expr

val is_empty :
  'a expr -> bool

val null :
  'a expr -> bool

val is_finite :
  char expr -> bool

val product :
  'a list list -> 'a list list -> 'a list list
val enumerate :
  char list -> char expr -> char list list option

val alphabet_expr :
  'a expr -> 'a list

type answer =
  Infinite | Accept | Reject
val accept_partial :
  char expr -> char list -> answer
