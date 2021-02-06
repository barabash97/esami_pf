(* ESERCIZIO 1 *)
let rec remove x = function 
	[] -> []
	|y::rest -> if y=x then remove x rest else y::remove x rest;;

let lista = [1;2;3;4;5;6];;

remove 3 lista;;

let remove_iter x = 
	let rec aux result = function 
		[] -> result
		|y::rest -> if y=x then aux result rest else aux (y::result) rest
	in aux [];;

remove_iter 6 (List.rev lista);;		 
 
(* ESERCIZIO 1 - ok *)


(* ESERCIZIO 2*)

type 'a graph = ('a * 'a) list;;

let grafo = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;

let rec vicini nodo = function 
	[] -> []
	| (a,b)::rest -> if a = nodo then b::vicini nodo rest
									else if b=nodo then a::vicini nodo rest
									else vicini nodo rest;;
 
let percorso g start tappa target = 
	let rec from_node visited node = 
		if List.mem node visited then failwith "from_node"
		else 
			if node = target && (List.mem tappa visited || node = tappa) then [node]
		else 
			node::from_list (node::visited) node (vicini node g)
		and from_list visited node = function 
			[] -> failwith "from_list"
			| x::rest -> try 
									from_node visited x
									with _ -> from_list visited x rest
									
	in from_node [] start;;

percorso grafo 1 6 7;;																				