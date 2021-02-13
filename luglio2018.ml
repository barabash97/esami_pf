type 'a ntree = Tr of 'a * 'a ntree list;;


(* ESERCIZIO 2*)

let ntree = Tr(1, 
						[
							Tr(2, 
									[
										Tr(5, []); 
										Tr(8, [Tr(9, [Tr(15, [])]);
										Tr(10, [])]) ]); 
										Tr(3, [Tr(6, []); Tr(7, []); Tr(18, [Tr(29,[]);Tr(4, [])])]);
										Tr(14, [Tr(19,[]); Tr(12, []); Tr(29, [Tr(13, [])])])
										
										]);;

ntree;;
 
(* depth: 'a -> 'a ntree -> int *)
let depth nodo t = 
	let rec from_node result t = match t with
	| Tr(x, tlist) -> 
		if x = nodo then result
		else 
			from_list (result+1) tlist
			and from_list result = function
				| [] -> failwith "from_list"
				| t::ts -> try (from_node result t) with _ -> (from_list result ts)
	in from_node 0 ntree;; 

depth 15 ntree;;

(* ESERCIZIO 2 OK *)

(* ESERCIZIO 3 *)


let parentelaOld t x y = 
	if depth x t = depth y t then (depth x t)
	else ((depth x t) + (depth y t));;

parentelaOld ntree 8 12;;

let rec parentela a b (Tr(x,tlist)) =
   try sc a b tlist
      with _  ->  depth b   (Tr(x,tlist)) + depth a (Tr(x,tlist))
and sc a b = function
 [] -> failwith "c"
   | x::rest -> try abs (depth a x - depth b x)
                      with _ -> sc a b rest;;

parentela 8 12 ntree;;
(* ESERCIZIO 3 FINE *)