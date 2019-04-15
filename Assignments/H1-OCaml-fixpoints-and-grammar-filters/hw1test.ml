(* Problem 1 *)
let my_subset_test0 = subset [] [1]
let my_subset_test1 = not (subset [1] [])

(* Problem 2 *)
let my_equal_sets_test0 = equal_sets [1;2] [2;1]
let my_equal_sets_test1 = not (equal_sets [1;2;3] [1;3])

(* Problem 3 *)
let my_set_union_test0 = equal_sets (set_union [1] [2;3]) [1;2;3]
let my_set_union_test1 = equal_sets (set_union [1;1;1;2;2] [1;2;3]) [1;2;3]

(* Problem 4 *)
let my_set_intersection_test0 = equal_sets (set_intersection [1;1;1;2;2] [1;2]) [1;2]
let my_set_intersection_test1 = equal_sets (set_intersection [1;1;1;2;2] []) []

(* Problem 5 *)
let my_set_diff_test0 = equal_sets (set_diff [1;2;3] [1;3]) [2]
let my_set_diff_test1 = equal_sets (set_diff [1;1;1;1] [1;2;3]) []

(* Problem 6 *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x*x - 3*x + 4) 2 = 2
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. 5.0) 2.0 = infinity

(* Problem 7 *)
type my_nonterminals = E | F | G | H 
let my_rules = 
  [E, [N F; N G];
   E, [N G; N F];
   E, [T"1"; T"1"];
   F, [N G];
   G, [N G];
   H, [N E]]
let my_grammar = H, my_rules

let my_filter_reachable_test0 = filter_reachable my_grammar = my_grammar
let my_filter_reachable_test1 = filter_reachable (F, my_rules) = 
  (F,
   [F, [N G];
    G, [N G]])
let my_filter_reachable_test2 = filter_reachable (E, my_rules) = 
  (E,
   [E, [N F; N G];
    E, [N G; N F];
    E, [T"1"; T"1"];
    F, [N G];
    G, [N G]])