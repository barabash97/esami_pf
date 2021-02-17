let lista = [1;2;3;4;5;6;7];;

(* ESERCIZIO 1 *)
let rec remove x = function 
	| [] -> []
	| y::rest -> if y=x then remove x rest else y::remove x rest;;

let remove_iter x lista = 
	let rec aux result = function 
		| [] -> result
		| y::rest -> if x = y then aux result rest else aux (y::result) rest

in List.rev(aux [] lista);;

(* ESERCIZIO 1 OK*)

type 'a graph = ('a * 'a) list;;
let g = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;

let rec successori x = function 
	| [] -> []
	| (a,b)::rest -> if a = x then b::successori x rest else successori x rest;;

let percorso g start tappa goal = 
	let rec from_node visited nodo = 
		if List.mem nodo visited then failwith "from_node"
		else if nodo = goal && (List.mem tappa visited || start=goal) then [nodo]
		else 
			nodo::from_list (nodo::visited) nodo (successori nodo g)
		and from_list visited nodo = function
			| [] -> []
			| x::rest -> try from_node visited x with _ -> from_list visited x rest

in from_node [] start;;

percorso g 1 4 5;; 