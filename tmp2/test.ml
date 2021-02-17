let figli x ntree =
	let rec from_node x result = function
		| Tr(t, []) -> result
		| Tr(t, tlist) -> 
					if t = x then from_list x (t::result) tlist
				and from_list x result = function
					| [] -> []
					| t::ts -> (from_node x result t)@(from_list x result ts)
in from_node x [] ntree;;