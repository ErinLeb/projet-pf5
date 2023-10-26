type base = A | C | G | T | WC
type dna = base list
val string_of_base : base -> string
val explode : string -> char list
val base_of_char : char -> base
val dna_of_string : string -> base list
val string_of_dna : dna -> string
val cut_prefix : 'a list -> 'a list -> 'a list option
val first_occ : 'a list -> 'a list -> ('a list * 'a list) option
val slices_between : 'a list -> 'a list -> 'a list -> 'a list list
val cut_genes : dna -> dna list
type 'a consensus = Full of 'a | Partial of 'a * int | No_consensus
val consensus : 'a list -> 'a consensus
val consensus_sequence : 'a list list -> 'a consensus list
