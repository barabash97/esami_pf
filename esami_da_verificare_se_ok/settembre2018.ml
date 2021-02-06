(* ESERCIZIO 1 *)

let lista_true = [1;2;3;4;5];;
let lista_false = [1;2;4;3;5];;

(* is_sorted: 'a list -> bool *)
(* aux: 'a -> 'a list -> bool *)
let is_sorted lista = 
	let rec aux last_value = function 
		| [] -> true
		| x::rest -> 
				   x >= last_value && aux x rest
	in aux (List.hd lista) (List.tl lista);;				  

is_sorted lista_true;;


(* ESERCIZIO 3 *)

exception ExceptionCamminoOrdinato;;

type 'a graph = ('a * 'a) list;;

let successori nodo graph = 
	List.map snd (List.filter(function (x,_) -> x = nodo) graph);;


let sorted_path (graph: 'a graph) start goal = 
	
	let rec from_node visited last_value node = 
		
		if List.mem node visited || last_value > node then raise ExceptionCamminoOrdinato
		else if node = goal then [node]
			else node::from_list (node::visited) node (successori node graph)
			
	and from_list visited last_value = function 
		
		| [] -> raise ExceptionCamminoOrdinato
		| x::rest -> 
			try 
				from_node visited last_value x
			with _ -> from_list visited x rest
			
	in start::from_list [start] start (successori start graph);;	

let grafo = [(1,2); (1,5); (2,4); (4,3); (3,7); (5,7)];;
sorted_path grafo 1 7;;			
																																																					
(* END Esercizio 3 -> OK *)

(* ESERCIZIO 2*)

exception NotFound;;

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

exception ExceptionSortedBranch;;



let albero = Tr(1, Tr(2, Tr(4, Empty, Empty), Tr(5, Empty,Empty)), Tr(3, Tr(8, Tr(6, Empty,Empty), Empty), Tr(7, Empty,Empty)));;

let root = function 
	Empty -> raise NotFound
	|Tr(x,_,_) -> x;;

let rec sorted_branch t x = match t with
| Empty -> raise NotFound
| Tr(k, Empty, Empty) -> if x = k then [k] else raise NotFound
| Tr(k, t1,t2) -> try 
									(if k < root t1 then k::sorted_branch t1 x else raise NotFound)
									with NotFound -> (
											if k < root t2 then k::sorted_branch t2 x else raise NotFound
										);;

sorted_branch albero 4;; 

(* OK ESERCIZIO 2*)


																																																																																																																																																																																																																																																													
																																																																																																																																																																																																																																																																																																															
																																																																																																																																																																																																																																																																																																																																																																	
																																																																																																																																																																																																																																																																																																																																																																																																																			
																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																																							
																																																																																																																																																										  				