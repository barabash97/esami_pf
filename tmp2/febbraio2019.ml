type color = Rosso | Verde | Neutro;;

let cols = [(2,Rosso); (3,Verde); (4,Verde);
(6,Verde); (7,Rosso)];;

let getcolor c lst = 
	try List.assoc c lst 
	with _ -> Neutro;;

let rec addset c = function 
	[] -> [(1, c)]
	| (n,colore)::rest -> if colore = c then ((n+1), colore)::rest else (n,colore)::addset c rest;;


let conta_colori cols lst = 
	let rec aux result = function 
		| [] -> result
		| x::rest -> aux (addset (getcolor x cols) result) rest
		in aux [] lst;;

conta_colori cols [1;2;3;4;5;6;7;8;9;10] ;;

type 'a graph = ('a * 'a) list;;

let grafo = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;

let rec successori n = function 
[] -> []
| (a,b)::rest -> if a = n then b::successori n rest
 							else if b=n then a::successori n rest
							else successori n rest;;

exception NotFound;;

let path g colors lst start =
	let rec from_node visited nodo lst = 
		if List.mem nodo visited then raise NotFound
		else let nodo_color = getcolor nodo colors in 
			if lst = [] || lst = [nodo_color] then [nodo]
			else let new_list = if List.hd lst = nodo_color then List.tl lst else lst in 
			nodo::from_list (nodo::visited) nodo new_list (successori n g)
			and from_list visited nodo lst = function
				| [] -> raise NotFound
				| x::rest -> try from_node visited x lst with NotFound -> from_list visited x lst rest

in from_node [] start lst;;

path g cols [Verde;Rosso] 1;;