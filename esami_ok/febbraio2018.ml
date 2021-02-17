let durata = [25;30;45];;

(* durations_to_end_times:  int list -> int list *)
let durations_to_end_times lst = 
	let rec aux result predvalue = function 
		| [] -> result
		| x::rest -> 
			let nuova_somma = predvalue + x in aux (nuova_somma::result) nuova_somma rest
	in List.rev(aux [] 0 lst);;

durations_to_end_times durata;;

(* ESERCIZIO 2 *)

type 'a graph = ('a * 'a) list;;

type 'a money = ('a * int) list;;

let money = [('C', -7); ('D', -15); ('F', 3); ('G', -5)];;

let grafo = [('A','C'); ('A', 'B'); ('A', 'D'); ('B', 'E'); ('C','E'); ('C','F'); ('D','F'); ('E','G'); ('F', 'G')];;

(* getnodewallet: 'a -> 'a money *)
let getnodewallet node money = 
	try 
		List.assoc node money
	with _ -> 0;;	

(* successori: 'a -> 'a graph -> 'a list *)
let rec successori nodo = function 
  [] -> []
  | (x,y)::rest -> 
    if x = nodo then y::successori nodo rest
    else successori nodo rest;;

exception NotFound;;

(* safe_path: 'a graph - 'a money -> 'a -> 'a -> 'a -> 'a list *)
let safe_path g wallet start goal init = 
	let rec from_node visited nodo cwallet = 
		if List.mem nodo visited then failwith "from_node"
		
		else let new_wallet = cwallet + getnodewallet nodo wallet in 
				
				if new_wallet < 0 then failwith "from_node"
				else if nodo = goal then [nodo] else 
					nodo::from_list (nodo::visited) nodo new_wallet (successori nodo g)
					
				and from_list visited nodo cwallet = function 
					| [] -> failwith "from_list"
					| x::rest -> try from_node visited x cwallet with _ -> from_list visited x cwallet rest
		in from_node [] start init;; 	
				
safe_path grafo money 'A' 'G' 10;;				