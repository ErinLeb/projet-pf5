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

let () = add_tests2
  "3.4"
  "cut_prefix"
  cut_prefix
  pp_list_char
  pp_list_char
  Alcotest.(option @@ list char)
  begin
    let abc = !!"abc" in
    [ [], [], Some ([]);
      [], abc, Some (abc);
      !!"a", abc, Some (!!"bc");
      !!"ab", abc, Some (!!"c");
      abc, abc, Some (!!"");
      !!"dc", abc, None;
      !!"adc", abc, None;
      !!"abd", abc, None;
    ]
  end

let () = add_tests2
  "3.5"
  "first_occ"
  first_occ
  pp_list_char
  pp_list_char
  Alcotest.(option @@ pair (list char) (list char))
  begin
    let abcd = !!"abcd" in
    [ [], [], Some ([], []);
      [], abcd, Some ([], abcd);
      !!"a", abcd, Some ([], !!"bcd");
      !!"ab", abcd, Some ([], !!"cd");
      !!"aa", !!"abaaacd", Some (!!"ab", !!"acd");
      !!"aaa", !!"abaaacd", Some (!!"ab", !!"cd");
      !!"be", abcd, None;
      !!"cde", abcd, None;
    ]
  end

let () = add_tests3
  "3.6"
  "slices_between"
  slices_between
  pp_list_char
  pp_list_char
  pp_list_char
  Alcotest.(list (list char))
  [ !!"ab", !!"cd", !!"ddcdeeffggabhhh", [];
    !!"ab", !!"cd", !!"ddcdeeffggcdhhh", [];
    !!"ab", !!"cd", !!"ddabeeffggabhhh", [];
    !!"ab", !!"cd", !!"ddabeecdffcdggabhhh", [!!"ee"];
    !!"ab", !!"cd", !!"ddabeecdffabggcdhh", [!!"ee"; !!"gg"];
    !!"ab", !!"cd", !!"ddabeecdffcdffabggcdhh", [!!"ee"; !!"gg"];
    !!"ab", !!"cd", !!"ddabeabecdffcdffabggcdhh", [!!"eabe"; !!"gg"];
    !!"aa", !!"cc", !!"ddaaaeaaecccffaaaffaaaggccchh", [!!"aeaae"; !!"affaaagg"];
  ]
    


  
