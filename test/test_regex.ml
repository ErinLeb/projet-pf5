open Regex_base
open Regex
open Test_base

let concat xs =
  List.fold_right (fun x acc -> Concat (x, acc)) xs Eps

let testable_expr =
  Alcotest.testable pp_expr (=)

let answer_to_string = function
  | Accept -> "Accept"
  | Reject -> "Reject"
  | Infinite -> "Infinite"
let pp_answer ppf answer =
  Format.pp_print_string ppf @@ answer_to_string answer
let testable_answer =
  Alcotest.testable pp_answer (=)

let () = add_tests2
  "4.1"
  "repeat"
  repeat
  Fmt.int
  pp_list_char
  Alcotest.(list char)
  [ 0, !!"", !!"";
    2, !!"", !!"";
    0, !!"a", !!"";
    1, !!"a", !!"a";
    3, !!"a", !!"aaa";
    0, !!"ab", !!"";
    1, !!"ab", !!"ab";
    3, !!"ab", !!"ababab";
  ]

let rec expr_normalize e =
  match e with
  | Eps ->
      []
  | Base _
  | Joker
  | Alt _
  | Star _ ->
      [e]
  | Concat (e1, e2) ->
      expr_normalize e1 @ expr_normalize e2
let () = add_tests2
  "4.2"
  "expr_repeat"
  (fun n e -> expr_normalize @@ expr_repeat n e)
  Fmt.int
  pp_expr
  (Alcotest.list testable_expr)
  [ (let e = Base 'a' in 0, e, []);
    (let e = Base 'a' in 1, e, [e]);
    (let e = Base 'a' in 3, e, [e; e; e]);
    (let e = Alt (Eps, Base 'a') in 0, e, []);
    (let e = Alt (Eps, Base 'a') in 1, e, [e]);
    (let e = Alt (Eps, Base 'a') in 3, e, [e; e; e]);
    (let e1 = Base 'a' in let e2 = Base 'b' in 0, Concat (e1, e2), []);
    (let e1 = Base 'a' in let e2 = Base 'b' in 1, Concat (e1, e2), [e1; e2]);
    (let e1 = Base 'a' in let e2 = Base 'b' in 3, Concat (e1, e2), [e1; e2; e1; e2; e1; e2]);
    (let e = Eps in 0, e, []);
    (let e = Eps in 1, e, []);
    (let e = Eps in 3, e, []);
  ]

let () = add_tests1
  "4.3"
  "is_empty"
  is_empty
  pp_expr
  Alcotest.bool
  [ Eps, true;
    Concat (Eps, Eps), true;
    Star Eps, true;
    Star (Star Eps), true;

    Base 'a', false;
    Joker, false;
    Concat (Eps, Joker), false;
    Alt (Eps, Joker), false;
    Alt (Joker, Eps), false;
    Alt (Joker, Joker), false;
    Star (Base 'a'), false;
  ]

let () = add_tests1
  "4.4"
  "null"
  null
  pp_expr
  Alcotest.bool
  [ Eps, true;
    Concat (Eps, Eps), true;
    Concat (Concat (Eps, Eps), Eps), true;
    Alt (Joker, Eps), true;
    Alt (Eps, Joker), true;
    Alt (Joker, Star Joker), true;
    Alt (Star Joker, Joker), true;
    Alt (Joker, Alt (Eps, Joker)), true;
    Alt (Alt (Eps, Joker), Joker), true;
    Concat (Alt (Joker, Eps), Eps), true;
    Concat (Alt (Joker, Eps), Concat (Eps, Eps)), true;
    Star Eps, true;
    Star Joker, true;
    Star (Concat (Joker, Eps)), true;

    Base 'a', false;
    Joker, false;
    Concat (Base 'a', Eps), false;
    Concat (Eps, Base 'a'), false;
    Concat (Star Joker, Joker), false;
    Concat (Joker, Star Joker), false;
    Alt (Joker, Joker), false;
  ]

let () = add_tests1
  "4.5"
  "is_finite"
  is_finite
  pp_expr
  Alcotest.bool
  [ Eps, true;
    Base 'a', true;
    Joker, true;
    Concat (Eps, Joker), true;
    Concat (Joker, Eps), true;
    Concat (Star Eps, Joker), true;
    Concat (Joker, Star Eps), true;
    Alt (Joker, Joker), true;
    Alt (Eps, Star Eps), true;
    Alt (Eps, Star (Star Eps)), true;
    Alt (Eps, Star (Alt (Eps, Eps))), true;
    Star Eps, true;
    Star (Concat (Eps, Eps)), true;
    Star (Alt (Eps, Eps)), true;

    Concat (Star Joker, Eps), false;
    Concat (Eps, Star Joker), false;
    Alt (Base 'a', Star Joker), false;
    Alt (Star Joker, Base 'a'), false;
    Star (Base 'a'), false;
    Star Joker, false;
    Star (Concat (Joker, Eps)), false;
    Star (Alt (Joker, Eps)), false;
    Star (Alt (Eps, Joker)), false;
  ]

let () = add_tests2
  "4.6"
  "product"
  (fun l1 l2 -> sort_uniq @@ product l1 l2)
  pp_list_list_char
  pp_list_list_char
  Alcotest.(list (list char))
  [ [], [], [];
    [!!""], [], [];
    [], [!!""], [];

    [!!"a"], [!!"b"], [!!"ab"];
    [!!"aa"], [!!"bb"], [!!"aabb"];

    [!!"a"; !!"b"], [!!"c"], [!!"ac"; !!"bc"];
    [!!"aa"; !!"bb"], [!!"cc"], [!!"aacc"; !!"bbcc"];

    [!!"a"], [!!"b"; !!"c"], [!!"ab"; !!"ac"];
    [!!"aa"], [!!"bb"; !!"cc"], [!!"aabb"; !!"aacc"];

    [!!"a"; !!"b"], [!!"c"; !!"d"], [!!"ac"; !!"ad"; !!"bc"; !!"bd"];
    [!!"aa"; !!"bb"], [!!"cc"; !!"dd"], [!!"aacc"; !!"aadd"; !!"bbcc"; !!"bbdd"];
  ]

let () = add_tests2
  "4.7"
  "enumerate"
  (fun alphabet e -> Option.map sort_uniq @@ enumerate alphabet e)
  pp_list_char
  pp_expr
  Alcotest.(option (list (list char)))
  [ !!"abc", Eps, Some [!!""];
    !!"abc", Concat (Base 'a', Joker), Some [!!"aa"; !!"ab"; !!"ac"];
    !!"abc", Star (Base 'a'), None;
  ]

let () = add_tests1
  "4.8"
  "alphabet_expr"
  alphabet_expr
  pp_expr
  Alcotest.(list char)
  [ Eps, !!"";
    Star (Concat (Base 'a', Joker)), !!"a";
    Alt (Base 'a', Base 'b'), !!"ab";
    Star (Concat (Base 'a', Base 'b')), !!"ab";
    Alt (Star Eps, concat [Base 'x'; Base 'e']), !!"ex";
    concat [Alt (Base 'a', Base 'b'); Base 'c'; Base 'c'; Alt (Base 'x',Eps)], !!"abcx";
    concat [Alt (Base 'a', Alt (Base 'b', Base 'c')); Base 'd'; Base 'e'; Star (Base 'x')], !!"abcdex";
  ]

let () = add_tests2
  "4.9"
  "accept_partial"
  accept_partial
  pp_expr
  pp_list_char
  testable_answer
  [ (* Infinite *)
    Star (Base 'a'), !!"a", Infinite;
    (* Epsilon *)
    Eps, !!"", Accept;
    Eps, !!"a", Reject;
    (* Concat *)
    Concat (Base 'a', Base 'b'), !!"ab", Accept;
    Concat (Base 'a', Base 'b'), !!"ba", Reject;
    Concat (Base 'a', Concat (Base 'a',Base 'b')), !!"aab", Accept;
    Concat (Base 'a', Concat (Base 'a',Base 'b')), !!"abb", Reject;
    Concat (Base 'a', Eps), !!"a", Accept;
    Concat (Base 'a', Eps), !!"", Reject;
    (* Alt *)
    Alt (Eps, Base 'c'), !!"", Accept;
    Alt (Eps, Base 'c'), !!"c", Accept;
    (* Hard *)
    concat [Alt (Base 'a', Base 'b'); Base 'c'; Base 'c'; Alt (Base 'a',Eps)], !!"acc", Accept;
    concat [Alt (Base 'a', Base 'b'); Base 'c'; Base 'c'; Alt (Base 'a',Eps)], !!"bcca", Accept;
    concat [Alt (Base 'a', Base 'b'); Base 'c'; Base 'c'; Alt (Base 'a',Eps)], !!"cca", Reject;
    concat [Alt (Base 'a', Base 'b'); Base 'c'; Base 'c'; Alt (Base 'a',Eps)], !!"x", Reject;
  ]
