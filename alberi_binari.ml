type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;; 

let rec size = function 
	| Empty -> 0
	| Tr(_, t1,t2) -> 1 + size t1 + size t2;;


let albero = Tr(1, 
Tr(2, Tr(4, Empty, Empty), Tr(5, Empty, Empty)), Tr(3, Tr(8, Tr(6, Empty, Empty), Empty), Tr(7, Empty,Empty)));;

let rec tree_exist p = function 
	| Empty -> false
	| Tr(n,t1,t2) ->
		 p n || tree_exist p t1 || tree_exist p t2;;


let rec raccogli = function 
	 Empty -> []
	| Tr(x,t1,t2) -> x::((raccogli t1)@(raccogli t2));;

raccogli albero;;

let rec foglie = function 
	Empty -> []
	|Tr(x,Empty,Empty) -> [x] 
	|Tr(x,t1,t2) -> 
		(foglie t1)@(foglie t2);;

foglie albero;;

let rec nodi_con_un_figlio = function 
	Empty | Tr(_,Empty, Empty) -> []
| Tr(x, Empty,t) | Tr(x, t, Empty) -> [x]::nodi_con_un_figlio t
| Tr(x,t1,t2) -> (nodi_con_un_figlio t1)@(nodi_con_un_figlio t2);;

nodi_con_un_figlio albero;;
		
exception NotFound;;

let rec path_to x = function 
	| Empty -> raise NotFound
	| Tr(n, Empty, Empty) -> if x = n then [n] else raise NotFound
	|	Tr(n,t1,t2) -> 
			n::(try 
				path_to x t1
			with NotFound -> path_to x t2);;	

path_to 6 albero;;

let rec add y = function 
	| [] -> [(y,1)]
	| (a,b)::rest -> 
		if a = y then (a, b+1)::rest else (a,b)::(add y rest);;

let count t = 
	let rec aux result = function
		Empty -> result
	|Tr(x,t1,t2) -> 
		aux(aux (add x result) t1) t2
	in aux [] t;;	


				
		
		