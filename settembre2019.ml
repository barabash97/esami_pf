let superset = [1;2;3;4;5;6];;

let set = [2;4;6];;

let no_set = [2;7;6];;

exception NotFound;;

let rec diff lst1 lst2 = 
	List.filter(function x -> not(List.mem x lst2)) lst1;;


let rec permutazione superset = function 
	| [] -> []
	| x::rest -> 
			if List.mem x superset then x::permutazione superset rest  else raise NotFound;;



let complemento superset lst = 
	let permut = (permutazione superset lst) in 
		diff superset permut;;

complemento superset set;;				

complemento superset no_set;;				

(* ESERCIZIO 2 *)

type 'a tree_bin = Empty | Tr of 'a * 'a tree_bin * 'a tree_bin;;

let albero_bin = Tr(5, Tr(7, Tr(3, Empty,Empty), Tr(9, Empty,Empty)), Tr(1, Tr(4,Empty,Empty), Tr(5,Empty,Empty)));;

(* labels : 'a tree_bin -> 'a list *)
let rec labels = function 
	| Empty -> []
	| Tr(x, l, r) -> x::((labels l) @ (labels r));;

let rec discendenti elem = function
    Empty -> []
  |Tr(x,y,z) ->
     if(x=elem)
     then labels (Tr(x,y,z))
     else discendenti elem y @ discendenti elem  z;;

discendenti 7 albero_bin;;