val add_tests :
  string -> unit Alcotest.test_case list -> unit
val run_tests :
  unit -> unit

val add_tests1 :
  string ->
  string -> ('a -> 'b) ->
  'a Fmt.t -> 'b Alcotest.testable ->
  ('a * 'b) list ->
  unit
val add_tests2 :
  string ->
  string -> ('a1 -> 'a2 -> 'b) ->
  'a1 Fmt.t -> 'a2 Fmt.t -> 'b Alcotest.testable ->
  ('a1 * 'a2 * 'b) list ->
  unit
val add_tests3 :
  string ->
  string -> ('a1 -> 'a2 -> 'a3 -> 'b) ->
  'a1 Fmt.t -> 'a2 Fmt.t -> 'a3 Fmt.t -> 'b Alcotest.testable ->
  ('a1 * 'a2 * 'a3 * 'b) list ->
  unit

val (!!) :
  string -> char list

val pp_list_char :
  char list Fmt.t
val pp_list_list_char :
  char list list Fmt.t
