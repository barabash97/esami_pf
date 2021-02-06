(* ESERCIZIO 1 *)

let lista = [25;30;45];;

(* durations_to_end_times: 'a list -> 'a list  *)
let durations_to_end_times lista = 
	let rec aux somma_tempi = function 
		| [] -> []
		| x::rest -> (x+somma_tempi)::aux (x+somma_tempi) rest
	in aux 0 lista;;

(* OKKK *)


(* ESERCIZIO 2 *)

type 'a graph = ('a * 'a) list;;

type 'a money = ('a * int) list;;

let money = [('C', -7); ('D', -15); ('F', 3); ('G', -5)];;

let grafo = [('A', 'B'); ('A','C'); ('A', 'D'); ('B', 'E'); ('C','E'); ('C','F'); ('D','F'); ('E','G'); ('F', 'G')];;

let changemoney actual diff = 
	if (actual + diff) > 0 then (actual+diff) else 0;;

let getprice_from_node node lista = 
	try 
		List.assoc node lista
	with _ -> 0;;	


(* successori: 'a -> 'a graph -> 'a list *)
let rec successori nodo = function 
  [] -> []
  | (x,y)::rest -> 
    if x = nodo then y::successori nodo rest
    else successori nodo rest;;


let safe_path grafo money init start goal = 
	
	let rec from_node visited wallet node = 
		if List.mem node visited then failwith "safe_path2"
		else let new_wallet = wallet + getprice_from_node node money in
			if new_wallet < 0 then failwith "safe_path2"
			else if node = goal then [node]
			else 
				node::from_list (node::visited) new_wallet (successori node grafo)
			
				and from_list visited wallet = function 
					| [] -> failwith "safe_path2"
					| x::rest -> try 
							from_node visited wallet x
							with _ -> from_list visited wallet rest
		in from_node [] init start;;							
			
	
	
	(* OKK *)
								 								
safe_path grafo money 10 'A' 'G';;
		
		