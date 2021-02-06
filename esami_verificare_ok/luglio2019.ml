type 'a tree_bin = Empty | Tr of 'a * 'a tree_bin * 'a tree_bin;;

let albero_bin = Tr(5, Tr(7, Tr(3, Empty,Empty), Tr(9, Empty,Empty)), Tr(1, Tr(4,Empty,Empty), Tr(5,Empty,Empty)));;

let somma_albero_bin = 
	let rec aux somma = function 
		Empty -> somma
		| Tr(x,t1,t2) -> x + (aux somma t1) + (aux somma t2)
	in aux 0;;

	
let root_tree = function
	Empty -> []
	|Tr(x,_,_) -> x;;

exception NotFound;;


(* n_ramo_bin: int n -> 'a tree_bin -> 'a list *)
let rec n_ramo_bin n = function
	Empty -> failwith "n_ramo_bin"
	|Tr(x, Empty, Empty) -> if x = n then [x] else failwith "n_ramo_bin"
	|Tr(x,t1,t2) -> if x > n then failwith "n_ramo_bin" else 
				x::(
					try 
						n_ramo_bin (n-x) t1
						with _ -> n_ramo_bin (n-x) t2
					);;

n_ramo_bin 11 albero_bin;;

type 'a ntree = Tr of 'a * 'a ntree list;;

let albero_n = Tr(5, [
	Tr(7, [Tr(3,[]); Tr(9,[]); Tr(12, [])]); Tr(1, [Tr(4,[]); Tr(5, []); Tr(10,[])]); Tr(3,[Tr(9,[]); Tr(7, []); Tr(10, [])])
	]);;

let rec n_ramo n = function 
	Tr(x, []) -> if n = x then [x] else failwith "n_ramo"
	|Tr(x, tlist) -> if x > n then failwith "n_ramo" else x::n_ramo_list (n-x) tlist
	
	and n_ramo_list n = function 
		[] -> failwith "n_ramo_list"
		| t::rest -> try n_ramo n t with _ -> n_ramo_list n rest;;				
		 
n_ramo 16 albero_n;; 

(* OK, esame luglio 2019 *)
	