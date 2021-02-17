type 'a ntree = Tr of 'a * 'a ntree list;;

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

let rec appartiene (Tr(t, tlist)) x = 
	if tlist = [] then x=t
	else 
		x=t || from_list tlist x
		and from_list tlist x = match tlist with 
		| [] -> false
		| t::ts -> (appartiene t x) || (from_list ts x);;

appartiene ntree 8;;

exception NotFound;;

let rec profondita ntree x = match ntree with 
| Tr(t, []) -> if x=t then 0 else raise NotFound
| Tr(t, tlist) -> if x = t then 0 else 1 + from_list tlist x

and from_list tlist x = match tlist with
| [] -> 0
| t::ts -> try profondita t x with _ -> from_list ts x;;

profondita ntree 29;;

let rec parentela t n1 n2 = match t with
| Tr(x, []) -> if x=n1 && x=n2 then 0 else raise NotFound
| Tr(x, tlist) -> try from_list n1 n2 tlist with _ -> (profondita t n1) + (profondita t n2)
									and from_list n1 n2 = function 
										| [] -> failwith "Non ci sono sottoalberi contenenti i due nodi"
										| t::ts -> if (appartiene t n1) && (appartiene t n2) then parentela t n1 n2 
															else raise NotFound;;

parentela ntree 6 4;;		
					
