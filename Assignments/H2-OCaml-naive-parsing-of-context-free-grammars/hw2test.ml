(* Type Declarations *)
let accept_all string = Some string

type brackets_nonterminals = 
  | Circle | Square

let brackets_grammar = 
  (Circle,
    function
      | Circle -> 
        [[T"("; T")"];
         [T"("; T")"; N Square];
         [T"("; N Circle; T")"];
         [N Square; T"("; T")"]]
      | Square -> 
        [[T"["; T"]"];
         [T"["; T"]"; N Circle];
         [T"["; N Square; T"]"];
         [N Circle; T"["; T"]"]])

let frag = ["("; "("; ")"; "["; "]"; "("; ")"; ")"]

(* Problem 5: make_matcher_test *)
let make_matcher_test = 
  ((make_matcher brackets_grammar accept_all frag) = Some [])

(* Problem 6: make_parser_test *)
let make_parser_test = 
  let make_parser_rs = make_parser brackets_grammar frag 
  in match make_parser_rs with
    | None -> false
    | Some tree ->
      if parse_tree_leaves tree = frag then true
      else false

(* let make_parser_test =
  ((make_parser brackets_grammar frag)
  = Some (Node (Circle,
    [
      Leaf "(";
      Node (Circle,
        [
          Leaf "("; 
          Leaf ")";
          Node (Square, 
            [
              Leaf "["; 
              Leaf "]"; 
              Node (Circle, 
                [
                  Leaf "("; 
                  Leaf ")"
                ]
              )
            ]
          )
        ]
      );
      Leaf ")"]))) *)
