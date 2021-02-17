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