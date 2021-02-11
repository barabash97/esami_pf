type 'a ntree = Tr of 'a * 'a ntree list;;

let ntree = Tr(10, [Tr(8, [Tr(20, []); Tr(10, [Tr(5, []); Tr(4, [])]); Tr(20, [])]); Tr(5, [Tr(2, [Tr(1, []); Tr(8, [])]); Tr(8, [Tr(3, []); Tr(10, [])])])]);;

ntree;;

let rec nodi t = match t with
|  Tr(x,tlist) ->
    x::nodi_list tlist
and nodi_list = function
    [] -> []
  | t::ts -> nodi t @ nodi_list ts;;
									

nodi ntree;;

let rec nodi_figli = function
	| [] -> []
	| Tr(x, tlist)::rest -> x::nodi_figli rest;;   

let addset x tlist = 
	if List.mem x tlist then tlist else x::tlist;;

let rec remove_duplicate nodo result = function 
	| [] -> result
	| x::rest -> if nodo = x then remove_duplicate nodo result rest  else remove_duplicate nodo (addset x result) rest;;  

let  figli y t = 
	let rec aux result y =  function
	| Tr(x, []) -> if x = y then x::result else result
| Tr(x, tlist) -> if x = y then from_list ((nodi_figli tlist)@result) y tlist else from_list result y tlist
	and from_list result y = function 
		| [] -> []
		| t::ts ->  (aux result y t)@(from_list result y ts)
	in (remove_duplicate y [] (aux [] y t));; 

figli 8 ntree;;


(* Grafo pesato *)

type 'a graph = ('a * 'a * int) list;;

let grafo = [("A", "B", 2); ("A", "D", 1); ("B", "B", 1); ("B", "C", 1); ("B", "E", 8); ("C", "A", 3); ("C", "E", 5); ("C", "D", 3); ("D", "C", 6); ("D", "E", 10)];;

let rec successori nodo = function 
	[] -> []
	| (a,b,_)::rest -> if a = nodo then b::successori nodo rest
	else successori nodo rest;;

let rec next_node_peso nodo = function 
	| [] -> []
	| (a,b,p)::rest -> if a = nodo then (b,p)::next_node_peso nodo rest else next_node_peso nodo rest;; 

next_node_peso "A" grafo;;

exception NotFound;;

let convert lst = 
	let rec aux (v, p) = function 
		| [] -> (List.rev v,p)
		| (a,b)::rest -> aux (a::v, b+p) rest
		in aux ([],0) lst;; 

(* wpath: 'a graph -> 'a -> 'a -> 'a -> ('a list * 'a)*)
let wpath g start goal pmax = 
	let rec from_node visited nodo peso =
		if List.mem nodo visited || peso > pmax then failwith "from_node"
		else 
			if nodo = goal then [(nodo,peso)] 
			else 
				(nodo, peso)::from_list (nodo::visited) nodo peso (next_node_peso nodo g)
			and from_list visited nodo peso = function
				| [] -> []
				| (x,p)::rest -> try 
											from_node visited x p
											with _ -> from_list visited x p rest
											
		in convert(from_node [] start 0);;										 	

wpath  grafo "A" "E" 10;;	

												(* OKK ESERCIZIO GRAFI *)												
			