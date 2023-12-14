open Regex_base

(* Renvoie l concaténé n fois avec lui même *)
let repeat n l = 
  if n <= 0 then [] else 
  let rec aux n res = 
    if n = 1 then res
    else aux (n-1) l@res 
  in aux n l;;

(* Renvoie une expression régulière qui reconnaît les mots formés de la concaténation de n mots reconnus par e *)
let expr_repeat n e =
  if n<= 0 then Eps else
  if n = 1 then e else
  let rec aux n res = 
    if n = 1 then res 
    else aux (n-1) (Concat (e, res))
    in aux n e ;;

(* Renvoie true si le langage reconnu par e ne contient que le mot vide, false sinon *)
let rec is_empty e = match e with
| Eps -> true
| Joker | Base _  -> false
| Star a -> is_empty a
| Concat (a,b) | Alt (a, b) ->
    is_empty a && is_empty b

(* Renvoie true si le mot vide est reconnu par e, false sinon *)
let rec null e = match e with
| Eps -> true
| Joker | Base _  -> false
| Star a -> true
| Concat (a,b) -> null a && null b
| Alt (a, b) -> null a || null b

(* Renvoie true si le langage reconnu par e est fini, false sinon *)
let rec is_finite e =
  match e with 
  | Eps | Joker | Base _ -> true
  | Concat (a,b) | Alt (a,b) -> (is_finite a) && (is_finite b)
  | Star x -> is_empty x

(* Renvoie l'ensemble des mots formés de la concaténation de l1 et l2 *)
let product l1 l2 =
  let rec aux res reste = 
    match reste with 
    [] -> res
    |a::r -> aux ((List.map (fun y -> a@y) l2)@res) r
  in aux [] l1
  ;;

(* Renvoie none si le langage reconnu par e sur alphabet est infini, Some l le langage reconnu sinon *)
let rec enumerate alphabet e =
  if not (is_finite e) then None
  else let rec aux e = match e with
  | Eps -> [[]]
  | Joker -> List.map (fun x -> [x]) alphabet
  | Base x  -> [[x]]
  | Star a -> []
  | Concat (a,b) -> product (aux a) (aux b)
  | Alt (a, b) -> (aux a) @ (aux b)
  in Some (List.sort_uniq compare (aux e))

(* Renvoie la liste triée des lettres de e *)
let alphabet_expr e =
  let rec aux e = match e with
  | Eps | Joker -> []
  | Base c -> [c]
  | Concat (a,b) | Alt (a,b) -> aux a @ aux b
  | Star s -> aux s
  in List.sort_uniq compare (aux e)

type answer =
  Infinite | Accept | Reject

(* Renvoie Infinite si le langage reconnu par e est infini, Accept s'il reconnait w, Reject sinon *)
let accept_partial e w =
  let alphabet_e = alphabet_expr e in
  let alphabet = List.sort_uniq compare (alphabet_e)@(w) in
  let langage = enumerate alphabet e in 
  match langage with 
    |None -> Infinite
    |Some l -> if (List.mem w l) then Accept else Reject