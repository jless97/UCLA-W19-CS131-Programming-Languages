(* Helper Functions *)
let contains_elem list elem = 
  List.exists (fun x -> elem = x) list;;

let excludes_elem list elem = 
  List.for_all (fun x -> elem <> x) list;;

(* Problem 1 *)
let rec subset a b = match a with
  | [] -> true 
  | head::tail -> (contains_elem b head) && (subset tail b);;

(* Problem 2 *)
let rec equal_sets a b = (subset a b) && (subset b a);;

(* Problem 3 *)
let rec set_union a b = match a with
  | [] -> b
  | head::tail -> if (contains_elem b head) then (set_union tail b)
                  else (set_union tail (head::b));;

(* Problem 4 *)
let set_intersection a b = List.filter (contains_elem b) a;;

(* Problem 5 *)
let set_diff a b = List.filter (excludes_elem b) a;;

(* Problem 6 *)
let rec computed_fixed_point eq f x =
  if eq (f x) x then x
  else computed_fixed_point eq f (f x);;

(* Problem 7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* Get non-terminal symbol on lhs of given rule *)
let get_lhs (x, _) = x;;

(* Get list of symbols on rhs of given rule *)
let get_rhs (_, x) = x;;

(* Get all non-terminal symbols from rhs *)
let rec filter_rhs rhs = match rhs with 
  | [] -> []
  | N head::tail -> head::(filter_rhs tail)
  | T head::tail -> filter_rhs tail;;

(* Get non-terminal symbols currently reachable *)
let rec get_curr_reachable rules curr = match rules with
  | [] -> curr
  | head::tail -> 
    let lhs = get_lhs head in

    (* Unprocessed non-terminal symbol, append any non-terminals in rhs to curr list *)
    if contains_elem curr lhs then 
      let rhs = filter_rhs (get_rhs head) in
      let next = set_union curr rhs in
      get_curr_reachable tail next
    (* Processed non-terminal symbol, skip *)
    else get_curr_reachable tail curr;;

(* Get all reachable non-terminal symbols *)
let rec get_all_reachable rules curr = 
  (* Get any non-terminal symbols one level down *)
  let next = get_curr_reachable rules curr in

  (* If non-terminal symbols list remains unchanged, done *)
  if equal_sets curr next then curr 
  (* Else, get any non-terminal symbols from new locations *)
  else get_all_reachable rules next;;

(* Filter out unreachable rules *)
let filter_out_unreachable rules start = 
  (* Get all reachable non-terminal symbols *)
  let reachable = get_all_reachable rules [start] in 

  (* Filter out rules that contains unreachable terminal symbols *)
  List.filter (fun x -> contains_elem reachable (get_lhs x)) rules;;

(* Return reachable rules *)
let filter_reachable g = 
  let start = get_lhs g in
  let rules = get_rhs g in
  (start, filter_out_unreachable rules start);;
