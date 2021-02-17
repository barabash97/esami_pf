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
let depth ntree y =
	let rec aux result y = function 
		| Tr(x, []) -> if x=y then result else raise NotFound
		| Tr(x, tlist) -> if x = y then result else from_list (result+1) y tlist
		and from_list result y = function
			| [] -> raise NotFound
			| t::ts -> try aux result y t with NotFound -> from_list result y ts

in aux 0 y ntree;;

depth ntree 5;;

let rec depth_rec ntree y = match ntree with
| Tr(x, []) -> if x = y then 0 else raise NotFound
| Tr(x, tlist) -> if x = y then 0 else 1 + from_list y tlist 
	and from_list y = function 
		| [] -> 0
		| t::ts -> try depth_rec t y with _ -> from_list y ts;;

depth_rec ntree 5;; 


let rec appartiene ntree n = match ntree with
| Tr(t, []) -> if t = n then true else false
| Tr(t, tlist) ->  t = n || from_list n tlist
	and from_list n = function 
		| [] -> false
		| t::ts -> appartiene t n || from_list n ts;;

appartiene ntree 5;;	

let rec parentela ntree n1 n2 = match ntree with
| Tr(t,[]) -> if t = n1 && t = n2 then 0 else raise NotFound
| Tr(t, tlist) -> try from_list n1 n2 tlist 
									with NotFound -> ((depth ntree n1) + (depth ntree n2))
		and from_list n1 n2 = function 
			| [] -> raise NotFound
			| t::ts -> if (appartiene t n1) && (appartiene t n2) then parentela t n1 n2
								else raise NotFound;;

parentela ntree 8 12;;

