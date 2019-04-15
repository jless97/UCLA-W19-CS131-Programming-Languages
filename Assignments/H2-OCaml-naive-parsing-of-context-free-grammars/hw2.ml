(* Type Declarations *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* Problem 1: convert_grammar *)

(* Function to retrieve the alternative list for a given nonterminal symbol using partial application *)
let rec get_alternative_list rules_list nonterminal = match rules_list with
  | [] -> []
  | head::tail -> if (fst head) = nonterminal then (snd head)::(get_alternative_list tail nonterminal)
                  else get_alternative_list tail nonterminal;;

let convert_grammar grammar =
  let start_symbol = fst grammar in
  let rules_list = snd grammar in 
  let production_function = get_alternative_list rules_list in
  (start_symbol, production_function);;

(* Problem 2: parse_tree_leaves *)

(* Helper function to build the list of leaves *)
let rec parse_tree_list list = match list with
  | [] -> []
  | head::tail -> match head with 
    | Leaf leaf -> leaf::(parse_tree_list tail)
    | Node (nonterminal, subtree) -> (parse_tree_list subtree) @ (parse_tree_list tail);;

let parse_tree_leaves tree = parse_tree_list [tree];;

(* Problem 3: make_matcher *)

let match_empty accept frag = accept frag;;
let match_nothing accept frag = None;;

let rec matcher production_function start_symbol = function 
  (* Current nonterminal alternative list is exhausted -- no prefix match found *)
  | [] -> match_nothing
  (* Current nonterminal alternative list has rules left to explore -- try to match prefix *)
  | rule_head::rule_tail -> 
    fun accept frag ->
      (* Try to find match for first rule in alternative list *)
      let head_matcher = current_rule_iterator production_function rule_head accept frag 
      (* Try to find match for subsequent rules in alternative list (if first rule fails) *)
      and tail_matcher = matcher production_function start_symbol rule_tail 
      in match head_matcher with
        (* No prefix match for first rule -- try subsequent rules in alternative list *)
        | None -> tail_matcher accept frag
        (* Prefix match for first rule -- return what acceptor returns *)
        | _ -> head_matcher
(* Helper function to match current rule (i.e. prefix) with frag by performing a linear scan *)
and current_rule_iterator production_function = function 
  (* Current rule is exhausted -- return what acceptor returns *)
  | [] -> match_empty
  (* Current rule head is nonterminal symbol -- DFS this symbol to try to find prefix match *)
  | (N nonterminal)::rule_tail -> 
    let rules = production_function nonterminal 
    in fun accept frag ->
      (* As we linear scan to match current rule with frag -- shift the suffix the acceptor will accept *)
      let new_accept = current_rule_iterator production_function rule_tail accept
      (* DFS next nonterminal symbol alternative list *)
      in matcher production_function nonterminal rules new_accept frag
  (* Current rule head is terminal symbol -- match with frag *)
  | (T terminal)::rule_tail -> (fun accept -> function
      (* Frag is exhausted -- but there are still symbols left to examine in current rule -- backtrack *)
      | [] -> None
      (* Frag has elems left to explore -- try matching with current rule elems *)
      | frag_head::frag_tail -> 
        (* Curr elems in frag and curr rule match -- recursively check if tails match *)
        if frag_head = terminal then current_rule_iterator production_function rule_tail accept frag_tail 
        (* Mismatch in curr rule and frag elems -- backtrack *)
        else None);;

let make_matcher grammar = 
  let start_symbol = fst grammar in
  let production_function = snd grammar in
  let rules = production_function start_symbol in
  fun accept frag ->
  matcher production_function start_symbol rules accept frag;; 

(* Problem 4: make_parser *)

let match_empty_2 accept derivation frag = accept derivation frag;;
let match_nothing_2 accept derivation frag = None;;
let accept derivation suffix = Some (derivation, suffix);;

let rec matcher_2 production_function start_symbol = function 
  (* Current nonterminal alternative list is exhausted -- no prefix match found *)
  | [] -> match_nothing_2
  (* Current nonterminal alternative list has rules left to explore -- try to match prefix *)
  | rule_head::rule_tail -> 
    (* Try to find match for first rule in alternative list *)
    fun accept derivation frag ->
      (* Try to find match for first rule in alternative list *)
      (* Main difference from original function -- retrieves the derivation for the matched prefix *)
      let head_matcher = current_rule_iterator_2 production_function rule_head accept (derivation @ [(start_symbol, rule_head)]) frag 
      (* Try to find match for subsequent rules in alternative list (if first rule fails) *)
      and tail_matcher = matcher_2 production_function start_symbol rule_tail 
      in match head_matcher with
        (* No prefix match for first rule -- try subsequent rules in alternative list *)
        | None -> tail_matcher accept derivation frag
        (* Prefix match for first rule -- return what acceptor returns *)
        | _ -> head_matcher
(* Helper function to match current rule (i.e. prefix) with frag by performing a linear scan *)
and current_rule_iterator_2 production_function = function 
  (* Current rule is exhausted -- return what acceptor returns *)
  | [] -> match_empty_2
  (* Current rule head is nonterminal symbol -- DFS this symbol to try to find prefix match *)
  | (N nonterminal)::rule_tail -> 
    let rules = production_function nonterminal 
    in fun accept derivation frag ->
      (* As we linear scan to match current rule with frag -- shift the suffix the acceptor will accept *)
      let new_accept = current_rule_iterator_2 production_function rule_tail accept
      (* DFS next nonterminal symbol alternative list *)
      in matcher_2 production_function nonterminal rules new_accept derivation frag
  (* Current rule head is terminal symbol -- match with frag *)
  | (T terminal)::rule_tail -> (fun accept derivation -> function
      (* Frag is exhausted -- but there are still symbols left to examine in current rule -- backtrack *)
      | [] -> None
      (* Frag has elems left to explore -- try matching with current rule elems *)
      | frag_head::frag_tail -> 
        (* Curr elems in frag and curr rule match -- recursively check if tails match *)
        if frag_head = terminal then current_rule_iterator_2 production_function rule_tail accept derivation frag_tail 
        (* Mismatch in curr rule and frag elems -- backtrack *)
        else None);;

(* Mutually recursive helper function to construct a parse tree from a given derivation *)
let rec make_parse_tree = function
  | (nonterminal, rule)::derivation_tail -> 
    (* Construct paths from current node to all possible leaves *)
    let paths = dfs_current_node derivation_tail rule 
    in match paths with 
      | (remaining_derivation, curr_to_leaves) ->
        (* Create node (i.e. tree) from current node to all possible leaves *) 
        let node = Node (nonterminal, curr_to_leaves) in
        remaining_derivation, node
and dfs_current_node remaining_derivation = function 
  | [] -> remaining_derivation, []
  (* Build out all parse subtree from current nonterminal *)
  | (N nonterminal)::rule_tail -> 
    (* Build out parse subtree from current elem of rules list *)
    (let curr_to_leaves = make_parse_tree remaining_derivation 
    in match curr_to_leaves with
      | (remaining_derivation_1, curr_paths) ->
        (* Build out parse subtrees from other elems of rules list *)
        let other_rules_to_leaves = dfs_current_node remaining_derivation_1 rule_tail 
        in match other_rules_to_leaves with
          | (remaining_derivation_2, other_rules_paths) ->
            let all_paths = (curr_paths::other_rules_paths) in
            (* Return (what's left of derivation yet to build, parse trees built from current node *)
            remaining_derivation_2, all_paths)
  (* Construct leaf from current terminal and build remaining parse subtrees *)
  | (T terminal)::rule_tail -> 
    (* Build out parse subtree from other elems of rule list *)
    (let subtrees = dfs_current_node remaining_derivation rule_tail
    in match subtrees with
      | (remaining_derivation_1, additional_paths) ->
        let leaf = ((Leaf terminal)::additional_paths) in
        (* Return (what's left of derivation yet to build, parse trees built from current node *)
        remaining_derivation_1, leaf);;

let make_parser grammar = 
  let start_symbol = fst grammar in
  let production_function = snd grammar in
  let rules = production_function start_symbol in
  fun frag -> 
    (* Build out the derivation for the given fragment, which will be used to construct the parse tree *)
    let derivation = matcher_2 production_function start_symbol rules accept [] frag
    in match derivation with
      | None -> None
      | Some (prefix, suffix) ->
        (* If the fragment can't be entirely parsed (i.e. some suffix left over), then return None *)
        if suffix != [] then None
        else 
          let parse_tree = make_parse_tree prefix
          in match parse_tree with
            | (_, tree) -> Some tree;; 
