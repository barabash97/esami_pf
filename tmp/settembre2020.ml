type 'a graph = ('a * 'a) list;;

let grafo = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;

let rec vicini nodo = function 
	| [] -> []
	| (a,b)::rest -> 
			if a = nodo then b::vicini nodo rest
			else if b=nodo then a::vicini nodo rest
			else vicini nodo rest;;

let percorso g start tappa target = 
	let rec from_node visited nodo = 
		if List.mem nodo visited then failwith "from_node"
		else if nodo = target && (List.mem tappa visited || tappa = target) then [nodo]
		else 
			nodo::from_list (nodo::visited) nodo (vicini nodo g)
		and from_list visited nodo = function 
			| [] -> failwith "from_list"
			| a::rest -> try 
												from_node visited a
											with _ -> from_list visited a rest
		in from_node [] start;;

percorso grafo 1 6 7;;
																					 	