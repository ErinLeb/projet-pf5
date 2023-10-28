open Adn
open Test_base

let () = add_tests1
  "3.0"
  "explode"
  explode
  Fmt.string
  Alcotest.(list char)
  [ "", [];
    "a" , ['a'];
    "ab" , ['a'; 'b'];
    "abc" ,['a'; 'b'; 'c'];
  ]

(* V. le pp ne peut être que nop... sauf à écrire ici la fonction demandée *)
let () = add_tests1
  "3.1"
  "base_of_char"
  base_of_char
  Fmt.char
  (Alcotest.testable Fmt.nop (=))
  [ 'A', A;
    'T', T;
    'C', C;
    'G', G;
    '.', WC;
    '$', WC
  ]
       
let () = add_tests1
  "3.2"
  "dna_of_string"
  dna_of_string
  Fmt.string
  (Alcotest.testable Fmt.nop (=))
  [ "", [];
    "ACTG.", [A; C; T; G; WC];
    "$GTCA", [WC; G; T; C; A]
  ]

let () = add_tests1
  "3.3"
  "string_of_dna"
  string_of_dna
  Fmt.nop
  (Alcotest.string)
  [ [], "";
    [A; C; T; G; WC], "ACTG.";
    [WC; G; T; C; A], ".GTCA" 
  ]


  
