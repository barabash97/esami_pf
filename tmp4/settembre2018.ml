let lista = [1;2;3;4;5];;

let is_sorted lst = 
	let rec aux result pred = function
		| [] -> result
		| x::rest -> if x > pred then aux (true) x rest else false
	in aux (true) (List.hd lst) (List.tl lst);;
 
is_sorted lista;;

type 'a treebin = Empty | Tr of 'a * 'a treebin * 'a treebin;;

let albero = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty,Empty)), Tr(3, Tr(8, Tr(6, Empty,Empty), Empty), Tr(7, Empty,Empty)));;

let root = function
	| Tr(k, _, _) -> k;;

exception NotFound;;
let rec sorted_branch t x = match t with
| Empty -> raise NotFound
| Tr(k,Empty, Empty) -> if k = x then [k] else raise NotFound
| Tr(k,l,r) -> try ( if k < (root l) then k::sorted_branch l x else raise NotFound)
							with NotFound -> (if k < (root r) then k::sorted_branch r x else raise NotFound);;

sorted_branch albero 4;;

type 'a graph = ('a * 'a) list;;

let grafo = [(1,2); (1,5); (2,4); (4,3); (3,7); (5,7)];;

let rec successori nodo = function 
	| [] -> []
	| (a,b)::rest -> if a = nodo then b::successori nodo rest else successori nodo rest;;
 

let sorted_path g start goal = 
	let rec from_node visited nodo pred = 
		if List.mem nodo visited || pred > nodo then failwith "from_node"
		else 
			if nodo = goal then [nodo]
			else nodo::from_list (nodo::visited) nodo (successori nodo g)
		and from_list visited pred = function 
			| [] -> failwith "from_list"
			| x::rest -> try from_node visited x pred with _ -> from_list visited pred rest
		in start::from_list [start] (start) (successori start g);;

sorted_path grafo 1 4;;			  		