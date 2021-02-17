type 'a tree = Tr of 'a * 'a tree list;;

let albero = Tr(1,[Tr(2,[Tr(5,[]);
Tr(8,[Tr(9,[Tr(5,[])]); Tr(10,[])])]);
Tr(3,[Tr(6,[]);
Tr(7,[]);
Tr(8,[Tr(9,[]); Tr(4,[])])]);
Tr(4,[Tr(9,[]);
Tr(10,[]);
Tr(9,[Tr(2,[])])])]);;

(* setadd: 'a -> 'a list -> 'a list *)
let setadd x lst = 
	if List.mem x lst then lst else x::lst;;

let  no_duplicati lst = 
	let rec aux result = function 
		| [] -> result 
		| x::rest -> if List.mem x rest then aux result rest else aux (x::result) rest
	in aux [] lst;;	 

let radici tree =

	let rec aux result tree = match tree with
	
		| Tr(t,[]) -> (setadd t result)
	| Tr(t, tlist) -> from_list (setadd t result) tlist 
	
	and from_list result = function 
		| [] -> []
		| t::ts -> (aux result t)@(from_list result ts)
	in no_duplicati(aux [] tree);;
	 
radici albero;;

let addarco (a,b) lst = 
	if (List.mem a lst) && ((List.assoc a lst) = b) then lst else (a,b)::lst;;

let archi tree = 
	let rec aux result tree genitore = match tree with 
	| Tr(t, []) -> [addarco (genitore,t) result]
	| Tr(t,tlist) -> 
			from_list (addarco (genitore,t) result) genitore tlist
			and from_list result = function
				| [] -> []
				| t::ts -> (aux result )
	  