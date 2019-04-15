(*example code for discussion section*)

(* Higher order function examples
*)
let double x = 2 * x
let square x = x * x

let doTwice f x = f (f x)

let quad   x = doTwice double x


(* Extract the commonalities out in those two functions.
 * Can we write a 'map' function which takes a function as
 * an argument and apply it on the list passed to it?	
*)
let rec inc_all (xs:int list) : int list = 
  match xs with 
  | [] -> []
  | hd::tl -> (hd+1)::(inc_all tl)

let rec square_all (xs:int list) : int list =
  match xs with
  | [] -> []
  | hd::tl -> (hd*hd)::(square_all tl)

(* A map function which takes a function as
 * an argument and apply it on the all elements in
 * the list passed to it
*)
let rec map (f:int->int) (xs:int list) : int list = 
match xs with 
| [] -> []
| hd::tl -> (f hd)::(map f tl);;

(*more general map*)
let rec map f xs = 
match xs with 
| [] -> []
| hd::tl -> (f hd)::(map f tl);;

(* Rewrite inc_all and square_all using this map function
*)
let inc x = x+1;;
let inc_all xs = map inc xs;;

let square y = y*y;;
let square_all xs = map square xs;;

let rec map f xs = 
match xs with 
| [] -> []
| hd::tl -> (f hd)::(map f tl);;





(* A function sums a list of integers
*)
let rec sum (l : int list) : int =
  match l with
    [] -> 0
  | f :: r -> f + (sum r)

(* A function concatenates a list of strings
*)
let rec concat (l : string list) : string =
  match l with
    [] -> ""
  | f :: r -> f ^ (concat r)





(* A function sums a list of integers with
 * accumultor.
*)
let rec sum' (acc : int) (l : int list) : int =
  match l with
    [] -> acc
  | f :: r -> sum' (acc + f) r

(* A function concatenates a list of strings
 * with accumultor.
*)
let rec concate' (acc : string) (l : string list) : string =
  match l with
    [] -> acc
  | f :: r -> concate' (acc ^ f) r


let rec fold_left (f : 'a -> 'b ->'a) (acc : 'a) (l : 'b list): 'a =
  match l with
    [] -> acc
  | h :: tl -> fold_left f (f acc h) tl

let suml (l : int list) : int =
	fold_left (fun acc x -> acc + x) 0 l

let concatel (l: string list) : string = 
	fold_left (fun acc x -> acc ^ x) "" l


(* Rewrite suml using currying*)
let sum2 = List.fold_left (fun a x -> x + a) 0

(* Rewrite concatel using curring*)
let concate2 = List.fold_left (fun a x -> a ^ x) ""


(* See the true power of functional programming?
 * If not convinced, try to rewrite functions in Java in 2 lines
*)
let sum3 = List.fold_left (+) 0
let concate3 = List.fold_left (^) ""



(*My own type*)
type optionalInt = 
| Something of int
| Nothing;;

(**)
Something 3;;

Nothing;;

(*Parameterized Types*)
type ('a) myOption = 
| MySome of 'a
| None;;


(*Write our own list type*)
type ('a) myList = 
      | Empty
      | Cons of 'a * 'a myList;;

(*write a summation func on this list*)
let rec summList m = match m with
        | Empty -> 0
        | Cons (f, r) -> f + summList r;;


(* Write a type for a tree, each node either has two branches and a value a, 
 * or it is a Leaf.
*)
type ('a) bintree =
      | Node of 'a * 'a bintree * 'a bintree
      | Leaf;;


(* Write Pre-Order(root, left, right) Traversal of bintree, output is list
 *
*)
let rec preorder btree = match btree with
        | Node (v, left, right) -> v :: ((preorder left) @ (preorder right)) 
        | Leaf -> [];;

preorder (Node (3, Node (4, Node(5, Leaf, Leaf), Node(6, Leaf, Leaf)), Leaf));;

(*Dictionaries with tuples*)
type ('key, 'value) dict = ('key * 'value) list

(*Write a getEmptyDictionary*)
let empty = [];;

(*How do we write put function?*)
let put1 key value dict = (key, value)::dict;;

let dictb = put1 42 "meow" empty;;

(*How do we write get function?*)
let rec get1 key dict = match dict with
| [] -> None
| (k, v) :: tl -> if k = key then MySome v else get1 key tl;;

let ans = get1 42 dictb;;

let ans = get1 9 dictb;;


(* What do both of these acceptors accept?*)
let accept_all derivation suffix = Some (derivation, suffix)

let accept_nonempty derivation = function
| suffix -> Some (derivation, suffix)
| [] -> None





(*debug using trace*)
let rec fib x = if x <= 1 then 1 else fib (x - 1) + fib (x - 2);;
#trace fib;;
fib 3;;

(*debug using ocamlc*)
let rec member (x:int) = function
[] -> false
| h::t -> if x=h then 
				true 
		  else 
		  		member x t;;

member 1 [1;2;3]



