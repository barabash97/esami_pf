type 'a tree_bin = Empty | Tr of 'a * 'a tree_bin * 'a tree_bin;;

let albero_bin = Tr(5, Tr(7, Tr(3, Empty,Empty), Tr(9, Empty,Empty)), Tr(1, Tr(4,Empty,Empty), Tr(5,Empty,Empty)));;

exception NotFound;;

(* n_ramo_bin: 'a -> a' tree_bin -> 'a list *)

let n_ramo_bin n tree = 
	let rec aux result somma = function
		| Empty -> result
		| Tr(t, Empty, Empty) -> let new_somma = somma + t in if new_somma = n then t::result else raise NotFound 
		| Tr(t, l, r) -> let new_somma = somma + t in 
		if new_somma < n then (try aux (t::result) new_somma l with NotFound -> aux (t::result) new_somma r) 
		else raise NotFound
		in aux [] 0 tree;;  

n_ramo_bin 15 albero_bin;;

type 'a ntree = Tr of 'a * 'a ntree list;;

let ntree = Tr(5, [
	Tr(7, [Tr(3,[]); Tr(9,[]); Tr(12, [])]); Tr(1, [Tr(4,[]); Tr(5, []); Tr(10,[])]); Tr(3,[Tr(9,[]); Tr(7, []); Tr(10, [])])
	]);;

(* n_ramo: 'a -> 'a ntree -> 'a list *)
let n_ramo n ntree = 
	let rec aux result somma = function 
		| Tr(t, []) -> let new_somma = somma + t in if new_somma = n then t::result else raise NotFound 
		| Tr(t, tlist) -> let new_somma = somma + t in if new_somma < n then from_list (t::result) new_somma tlist else raise NotFound 

and from_list result somma = function 
	| [] -> raise NotFound
	| t::ts -> try aux result somma t with NotFound -> from_list result somma ts

in aux [] 0 ntree;;

n_ramo 15 ntree;;