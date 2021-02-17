type 'a ntree = Tr of 'a * 'a ntree list;;

let albero = Tr(10, 
									[
										Tr(8, [Tr(20, []); Tr(10, [Tr(5, []); Tr(4, [])]); Tr(20, [])]);
										Tr(5, [Tr(2, [Tr(1, []); Tr(8, [])]); Tr(8, [Tr(3, []); Tr(10, [])])])
									]);;
									
albero;;									


(* addset: 'a -> 'a list -> 'a list *)
let addset x lst = 
	if List.mem x lst then lst else x::lst;;

(* mkset -> 'a list -> 'a list -> 'a list *)
let rec mkset result = function 
	| [] -> result
	| x::rest -> remove_duplicates (addset x result) rest;; 


(* nodi: 'a ntree -> 'a list *)
let nodi t = 
	let rec from_node result = function
		| Tr(t, []) -> (addset t result)
		| Tr(t, tlist) -> 
		from_list (addset t result) tlist
		and from_list result = function 
			| [] -> []
			| t::ts -> (from_node result t)@(from_list result ts)
	in mkset [] (from_node [] t);;
nodi albero;;

(* prendereValoriLista: 'a ntree -> 'a list *)
let rec prendereValoriLista = function
	| [] -> [] 
	| Tr(t, tlist)::rest -> t::prendereValoriLista rest;;
 

(* merge: 'a list -> 'a list -> 'a list *)
let rec merge lst1 = function
	[] -> lst1
	| x::rest -> merge (x::lst1) rest;;

(* figli: 'a -> 'a ntree -> 'a list *)
let figli x ntree = 
	let rec from_node x result = function
	| Tr(t, []) -> result
	| Tr(t, tlist) -> if t = x then from_list x (merge (prendereValoriLista tlist) result) tlist
										else from_list x result tlist
		and from_list x result = function 
			|[] -> []
			| t::ts -> (from_node x result t)@(from_list x result ts)
	in mkset [] (from_node x [] ntree);;

figli 8 albero;;													 									

(* OK ESERCIZIO 1A E 1B*)

type 'a graph = ('a * 'a * int) list;;

let grafo = [("A", "B", 2); ("A", "D", 1); ("B", "B",1); ("B", "C", 1); ("B", "E", 8); ("C", "A", 3); ("C", "D", 3); ("C", "E", 5); ("D", "C", 6); ("D", "E", 10)];;

(* successori: 'a -> 'a graph -> (string * int) list *)
let rec successori x = function
	| [] -> []
	| (a,b,c)::rest -> if a = x then (b,c)::successori x rest else successori x rest;;

let rec somma_nodi_pesi nodi totsum  = function 
	| [] -> [nodi, totsum]
	| (a,p)::rest -> somma_nodi_pesi (a::nodi) (p+totsum) rest;; 

let wpath g start goal pesomax = 
	let rec from_node visited nodo ptot pcurrent =
		if List.mem nodo visited then failwith "from_node"
		else if nodo = goal then [(nodo, pcurrent)]
		else 
			(nodo,pcurrent)::from_list (nodo::visited) nodo ptot (successori nodo g)
			and from_list visited nodo ptot = function 
				| [] -> failwith "from_list"
				| (a,p)::rest -> let npeso = (ptot - p) in 
													if npeso < 0 then failwith "from_list" else 
														try from_node visited a npeso p with _ -> from_list visited a npeso rest
			in somma_nodi_pesi [] 0 (from_list [start] start pesomax (successori start g));;



wpath grafo "A" "E" 8;;																						 
