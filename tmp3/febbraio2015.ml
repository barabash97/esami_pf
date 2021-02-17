type 'a ntree = Tr of 'a * 'a ntree list;;

let ntree = Tr(10, [Tr(8, [Tr(20, []); Tr(10, [Tr(5, []); Tr(4, [])]); Tr(20, [])]); Tr(5, [Tr(2, [Tr(1, []); Tr(8, [])]); Tr(8, [Tr(3, []); Tr(10, [])])])]);;

let mkset lst =
	let rec aux result = function  
		| [] -> result
		| x::rest -> if List.mem x result then aux result rest else aux (x::result) rest
	in aux [] lst;;	 

let  nodi (Tr(t, tlist)) = 
	let rec aux result = function 
		| Tr(t, []) -> t::result
		| Tr(t, tlist) -> from_list (t::result) tlist
	
	and from_list result = function 
		| [] -> []
		| t::ts ->(aux result t)@(from_list result ts)

  in mkset(aux [] (Tr(t, tlist)));;

 nodi ntree;;

let figli x tree = 
	let rec aux result = function 
		| Tr(t, tlist) -> if t=x then nodi (Tr(t, tlist)) else from_list result tlist
	and from_list result = function 
		| [] -> []
		| t::ts -> (aux result t)@(from_list result ts)

	in mkset(aux [] tree);;

figli 8 ntree;;