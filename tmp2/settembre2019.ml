type 'a treebin = Empty | Tr of 'a * 'a treebin * 'a treebin;;


let albero = Tr(5, Tr(7, Tr(3, Empty,Empty), Tr(9, Empty,Empty)), Tr(1, Tr(4,Empty,Empty), Tr(5,Empty,Empty)));;

(* labels: 'a treebin -> 'a list *)
let rec labels = function 
	| Empty -> []
	| Tr(x, t1,t2) -> x::((labels t1)@(labels t2));;

let rec discendenti x = function
	| Empty -> []
	| Tr(y, t1,t2) -> if x = y then (labels (Tr(y,t1,t2))) else ((discendenti x t1) @ (discendenti x t2));;

discendenti 5 albero;;


let superset = [1;2;3;4;5;6];;
let set = [2;4;6];;

exception NotFound;;
let rec check superset = function
	| [] -> true
	| x::rest -> if List.mem x superset then check superset rest else raise NotFound;;


let rec remove x = function 
	| [] -> []
	| y::rest -> if x = y then remove x rest else y::remove x rest;;


let complemento superset set =
	let rec aux result = function 
		| [] -> result
		| x::rest -> if check superset [x] then aux (remove x result) rest else raise NotFound

	in aux superset set;;

let rec complemento_b superset = function 
	 [] -> superset
	| x::rest -> if check superset [x] then complemento_b (remove x superset) rest else raise NotFound;;
	

complemento_b superset set;;