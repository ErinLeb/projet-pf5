let list_of_string str =
  String.fold_right List.cons str []
let string_of_list chaine =
  let bytes = Bytes.create @@ List.length chaine in
  List.iteri (fun i c -> Bytes.set bytes i c) chaine ;
  String.of_bytes bytes

type 'a expr =
  | Eps
  | Base of 'a
  | Joker
  | Concat of 'a expr * 'a expr
  | Alt of 'a expr * 'a expr
  | Star of 'a expr

type pp_level =
  | PpGround
  | PpConcat
  | PpAlt
  | PpStar
let pp_expr_level = function
  | Eps
  | Base _
  | Joker ->
      PpGround
  | Concat _ ->
      PpConcat
  | Alt _ ->
      PpAlt
  | Star _ ->
      PpStar
let pp_level_protect lvl1 lvl2 =
  match lvl1, lvl2 with
  | PpGround, _ ->
      true
  | _, PpGround ->
      false
  | PpConcat, PpConcat ->
      false
  | PpConcat, PpStar ->
      false
  | PpConcat, PpAlt ->
      true
  | PpAlt, PpConcat ->
      false
  | PpAlt, PpStar ->
      false
  | PpAlt, PpAlt ->
      false
  | PpStar, PpConcat ->
      true
  | PpStar, PpStar ->
      false
  | PpStar, PpAlt ->
      true
let rec pp_expr ppf e =
  let lvl = pp_expr_level e in
  match e with
  | Eps ->
      Format.pp_print_string ppf "ε"
  | Base c ->
      Format.pp_print_string ppf
        begin match c with
        | '?' -> "\\?"
        | '+' -> "\\+"
        | '*' -> "\\*"
        | '(' -> "\\("
        | ')' -> "\\)"
        | _ -> Char.escaped c
        end
  | Joker ->
      Format.pp_print_char ppf '?'
  | Concat (e1, e2) ->
      Format.fprintf ppf "%a%a"
        (pp_expr_protect lvl) e1
        (pp_expr_protect lvl) e2
  | Alt (e1, e2) ->
      Format.fprintf ppf "%a+%a"
        (pp_expr_protect lvl) e1
        (pp_expr_protect lvl) e2
  | Star e ->
      Format.fprintf ppf "%a*"
        (pp_expr_protect lvl) e
and pp_expr_protect lvl ppf e =
  if pp_level_protect lvl @@ pp_expr_level e then (
    Format.fprintf ppf "(%a)"
      pp_expr e
  ) else (
    pp_expr ppf e
  )

let expr_to_string =
  Format.asprintf "%a" pp_expr

let sort_uniq l =
  List.sort Stdlib.compare l
let rec union_sorted l1 l2 =
  match l1, l2 with
  | _, [] -> l1
  | [], _ -> l2
  | a1 :: l1', a2 :: l2' ->
      if a1 < a2 then
        a1 :: union_sorted l1' l2
      else if a2 < a1 then
        a2 :: union_sorted l1  l2'
      else
        a1 :: union_sorted l1' l2'
