exception NotFound;;

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let albero = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty,Empty)), Tr(3, Tr(8, Tr(6, Empty,Empty), Empty), Tr(7, Empty,Empty)));;

(* root: 'a tree -> int *)
let root = function 
	| Empty -> raise NotFound
	| Tr(t, _,_) -> t;;

(* sorted_branch: 'a tree -> 'a -> 'a list *)
let rec sorted_branch t x = match t with
| Empty -> raise NotFound
| Tr(k, Empty, Empty) -> if k = x then [x] else raise NotFound
| Tr(k, t1,t2) -> try if k < root t1 then k::sorted_branch t1 x else raise NotFound
									with _ -> if k < root t2  then k::sorted_branch t2 x else raise NotFound;;

sorted_branch albero 4;;

type 'a graph = ('a * 'a) list;;
let grafo = [(1,2); (1,5); (2,4); (4,3); (3,7); (5,7)];;

let rec successori x = function
	| [] -> []
	| (a,b)::rest -> if a = x then b::successori x rest else successori x rest;;

let sorted_path g start goal = 
	let rec from_node visited nodo predvalue = 
		if List.mem nodo visited || predvalue > nodo then failwith "from_node"
		else if nodo = goal then [nodo]
		else 
			nodo::from_list (nodo::visited) predvalue (successori nodo g)
			and from_list visited predvalue = function 
				| [] -> failwith "from_list" 
				| x::rest -> try from_node visited x predvalue with _ -> from_list visited predvalue rest

	in start::from_list [start] start (successori start g);;

sorted_path grafo 1 4;;


