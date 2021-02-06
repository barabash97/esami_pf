(* ESERCIZIO 1 *)
let superset =  [1;2;3;4;5;6];;
let set =  [2;4;6];;
let no_set =  [2;7;6];;

exception NotFound;;

let rec check superset = function 
	[] -> true
	|x::rest -> if List.mem x superset then true && check superset rest else false;;

let diff l1 l2 = List.filter(function x -> not(List.mem x l2)) l1;;

let rec completamento superset set = 
	if check superset set then diff superset set else raise NotFound;;

completamento superset set;;
completamento superset no_set;;

(* END ESERCIZIO 1 - OK *)

(* ESERCIZIO 2 *)

type 'a ntree_bin = Empty | Tr of 'a * 'a ntree_bin * 'a ntree_bin;;

let albero_bin = Tr(5, Tr(7, Tr(3, Empty,Empty), Tr(9, Empty,Empty)), Tr(1, Tr(4,Empty,Empty), Tr(5,Empty,Empty)));;

let rec labels = function 
	Empty -> failwith "labels"
	|Tr(x, Empty, Empty) -> [x]
	|Tr(x,t1,t2) -> x:: ((labels t1)@(labels t2));;

let rec discendenti elem = function
    Empty -> []
  |Tr(x,y,z) ->
     if(x=elem)
     then labels (Tr(x,y,z))
     else discendenti elem y @ discendenti elem  z;;


discendenti 5 albero_bin;;

(* ESERCIZIO 2 - OK *)
		 