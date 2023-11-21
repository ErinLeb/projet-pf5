type base = A | C | G | T | WC (* wildcard *)

type dna = base list



(*---------------------------------------------------------------------------*)
(*                               ECHAUFFEMENT                                *)
(*---------------------------------------------------------------------------*)


let string_of_base (b : base) : string =
  match b with
    A -> "A"
  | C -> "C"
  | G -> "G"
  | T -> "T"
  | WC -> "."


(* explode a string into a char list *)
let explode (str : string) : char list =
  let rec aux i char_list = 
    if i < 0 then char_list else aux (i-1) (str.[i]::char_list)
    in aux (String.length str - 1) []


(* conversions *)
let base_of_char (c : char) : base =
  match c with
    'A' -> A
  | 'C' -> C
  | 'G' -> G
  | 'T' -> T
  | _ -> WC


let dna_of_string (s : string) : base list =
  let rec aux char_list dna = 
    match char_list with 
     [] -> List.rev dna 
    | c::r -> aux r (base_of_char c::dna)
    in aux (explode s) [] 


let string_of_dna (seq : dna) : string =
  List.map string_of_base seq |> String.concat ""

(*---------------------------------------------------------------------------*)
(*                                   SLICES                                  *)
(*---------------------------------------------------------------------------*)
(*
   Une {\em tranche} de $l = \langle x_1,\dots x_n\rangle$ est une sous-liste
   de $l$ de la forme $\langle x_i, \dots x_{j}$, o\`u $1 \leq i \leq j\leq n$.
 *)

(* if list = pre@suf, return Some suf. otherwise, return None *)
let rec cut_prefix (pre : 'a list) (list : 'a list) : 'a list option =
  match pre, list with
  | [], l -> Some list
  | hd::tl, hd'::tl' when hd=hd' -> cut_prefix tl tl'
  | _,_ -> None

(*
  cut_prefix [1; 2; 3] [1; 2; 3; 4] = Some [4]
  cut_prefix [1; 2; 3; 4] [1; 2; 3; 4] = Some []
  cut_prefix [1; 2; 0] [1; 2; 3; 4] = None
 *)


(* return the prefix and the suffix of the first occurrence of a slice,
   or None if this occurrence does not exist.
*)
let rec first_occ (slice : 'a list) (list : 'a list)
    : ('a list * 'a list) option =
  match list, cut_prefix slice list with
  | _, Some suf -> Some ([], suf)
  | [], None -> None
  | hd::tl, None -> Option.map  (fun (pre,suf) -> hd::pre,suf) (first_occ slice tl)
(*
  first_occ [1; 2] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([1; 1], [3; 4; 1; 2])
  first_occ [1; 1] [1; 1; 1; 2; 3; 4; 1; 2] = Some ([], [1; 2; 3; 4; 1; 2])
  first_occ [1; 3] [1; 1; 1; 2; 3; 4; 1; 2] = None
 *)


let rec slices_between (start : 'a list) (stop : 'a list) (list : 'a list) : 'a list list =
  let rec aux rest slices = 
    match first_occ start rest with
    | None -> List.rev slices
    | Some (pre,suf)-> match first_occ stop suf with 
                    | None -> List.rev slices
                    | Some (pre,suf) -> aux suf (pre::slices)
  in aux list []

(*
  slices_between [1; 1] [1; 2] [1; 1; 1; 1; 2; 1; 3; 1; 2] = [[1]; []; [2; 1; 3]]
 *)

let cut_genes (dna : dna) : (dna list) = slices_between [A; T; G] [T;A;A] dna

(*---------------------------------------------------------------------------*)
(*                          CONSENSUS SEQUENCES                              *)
(*---------------------------------------------------------------------------*)


type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus

(* return (Full a) if all elements of the list are equal to a,
   (Partial (a, n)) if a is the only element of the list with the
   greatest number of occurrences and this number is equal to n,
   No_consensus otherwise. *)
let consensus (list : 'a list) : 'a consensus =
  (* We could use a hashtable for better performance here *)
  if list = [] then No_consensus
  else
    let repartitions = List.fold_left (fun acc x -> match List.assoc_opt  x acc with
      | Some n -> (x, n+1)::(List.remove_assoc  x acc)
      | None -> (x, 1)::acc) [] list in
    if List.length repartitions = 1 then Full (fst @@ List.hd repartitions)
    else fst @@ List.fold_left  (fun (state, count) (item, count') ->
        if count = count' then (No_consensus, count)
        else if count < count' then (Partial(item,count'), count')
        else (state, count)) (No_consensus, 0) repartitions


(*
   consensus [1; 1; 1; 1] = Full 1
   consensus [1; 1; 1; 2] = Partial (1, 3)
   consensus [1; 1; 2; 2] = No_consensus
 *)

(* return the consensus sequence of a list of sequences : for each position
   in the elements of ll, compute the consensus  of the set of values at this
   position  in the sequences. the lists must be of same length. if all lists
   are empty, return the empty sequence.
 *)

let rec consensus_sequence (ll : 'a list list) : 'a consensus list =
    if List.for_all ((=) []) ll then []
    else if List.exists ((=) []) ll then
      failwith "All lists must be of the same length"
    else
        let acc, rest = List.fold_left_map
            (fun acc x -> List.hd x::acc, List.tl x) [] ll
        in consensus acc :: consensus_sequence rest


(*
 consensus_sequence [[1; 1; 1; 1];
                     [1; 1; 1; 2];
                     [1; 1; 2; 2];
                     [1; 2; 2; 2]]
 = [Full 1; Partial (1, 3); No_consensus; Partial (2, 3)]

 consensus_sequence [[]; []; []] = []
 *)
