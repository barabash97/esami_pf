let lista = [1;2;3;4;5];;

let is_sorted lst =
	let rec aux result pred = function 
		| [] ->true
		| x::rest -> if pred < x then aux (true) x rest else false

in aux true (List.hd lst) (List.tl lst);;

is_sorted lista;;


type 'a treebin = Empty | Tr of 'a * 'a treebin * 'a treebin;;


let albero = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty,Empty)), Tr(3, Tr(8, Tr(6, Empty,Empty), Empty), Tr(7, Empty,Empty)));;

(* visita: 'a treebin -> 'a list *)
let rec visita = function 
	| Empty -> []
	| Tr(t, t1,t2) -> t::((visita t1)@(visita t2));;

(* root: 'a treebin -> int *)
let root = function 
	| Tr(x, _, _) -> x;;

exception NotFound;;

(* sorted_branch: 'a treebin -> 'a -> 'a list *)
let rec sorted_branch t y = match t with
| Empty -> raise NotFound
| Tr(k, Empty, Empty) -> if k = y then [k] else raise NotFound
| Tr(k, l, r) -> try if k < (root l) then k::sorted_branch l y else raise NotFound 
								with NotFound -> if k < (root r) then k::sorted_branch r y else raise NotFound;;


sorted_branch albero 5;;


type 'a graph = ('a * 'a) list;;

let grafo = [(1,2); (1,5); (2,4); (4,3); (3,7); (5,7)];;


(* successori: 'a -> 'a graph -> 'a list *)
let rec successori x = function 
	| [] -> []
	| (a,b)::rest -> if a = x then b::successori x rest else successori x rest;; 

let sorted_path g start goal = 
	let rec from_node visited nodo pred = 
		if List.mem nodo visited then raise NotFound
		else if nodo = goal then [nodo]
		else if nodo < pred then raise NotFound
		else nodo::from_list (nodo::visited) nodo (successori nodo g)
		and from_list visited nodo = function 
			| [] -> failwith "from_node"
			| x::rest -> try from_node visited x nodo with NotFound -> from_list visited nodo rest

	in start::from_node [start] (List.hd (successori start g)) start;;

sorted_path grafo 1 4;;