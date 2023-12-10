open Regex_base

let repeat n l = 
  if n <= 0 then [] else 
  let rec aux n res = 
    if n = 1 then res
    else aux (n-1) l@res 
  in aux n l;;


let expr_repeat n e =
  if n<= 0 then Eps else
  if n = 1 then e else
  let rec aux n res = 
    if n = 1 then res 
    else aux (n-1) (Concat (e, res))
    in aux n e ;;

let rec is_empty e = match e with
| Eps -> true
| Joker | Base _  -> false
| Star a -> is_empty a
| Concat (a,b) | Alt (a, b) ->
    is_empty a && is_empty b

let rec null e = match e with
| Eps -> true
| Joker | Base _  -> false
| Star a -> true
| Concat (a,b) -> null a && null b
| Alt (a, b) -> null a || null b

let rec is_finite e =
  match e with 
  | Eps | Joker | Base _ -> true
  | Concat (a,b) | Alt (a,b) -> (is_finite a) && (is_finite b)
  | Star x -> is_empty x



let product l1 l2 =
  let rec aux res reste = 
    match reste with 
    [] -> res
    |a::r -> aux ((List.map (fun y -> a@y) l2)@res) r
  in aux [] l1
  ;;

let enumerate alphabet e =
  failwith "À compléter"

let rec alphabet_expr e =
  failwith "À compléter"

type answer =
  Infinite | Accept | Reject

let accept_partial e w =
  failwith "À compléter"
