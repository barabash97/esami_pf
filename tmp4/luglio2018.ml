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

exception NotFound;;

let depth x albero =
	let rec from_node result albero = match albero with
	| Tr(k, tlist) -> if k = x then result else 
		from_list (1+result) tlist
		and from_list result = function 
			| [] -> failwith "from_list"
			| t::ts -> try from_node result t with _ -> (from_list result ts)
	in from_node 0 albero;;

depth 1 ntree;;

let rec appartiene x albero = match albero with 
| Tr(k, []) -> k = x
| Tr(k, tlist) -> (k = x)||(from_list x tlist)
	and from_list x = function 
		| [] -> failwith "from_list" 
		| t::ts -> (appartiene x t) || (from_list x ts);;

let rec  parentela_b n1 n2 albero = match albero with 
| Tr(k, []) -> if k=n1 && k=n2 then 0 else raise NotFound
| Tr(k, tlist) -> try from_list n1 n2 tlist 
									with NotFound -> ((depth n1 albero) + (depth n2 albero))
			and from_list n1 n2 = function 
				[] ->  raise NotFound
				| t::ts ->  if (appartiene n1 t)&&(appartiene n2 t) then parentela_b n1 n2 t else raise NotFound;;
					

let rec parentela t n1 n2 = match t with
| Tr(x, []) -> if x=n1 && x=n2 then 0 else raise NotFound
| Tr(x, tlist) -> try from_list n1 n2 tlist with _ -> (depth n1 t) + (depth n2 t)
									and from_list n1 n2 = function 
										| [] -> failwith "Non ci sono sottoalberi contenenti i due nodi"
										| t::ts -> if (appartiene n1 t) && (appartiene n2 t) then parentela t n1 n2 
															else raise NotFound;;															

parentela_b 1 2 ntree;;					 
parentela ntree 1 2;;	 