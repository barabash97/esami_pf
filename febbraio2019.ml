type color = Rosso | Verde | Neutro;;

let cols = [(2,Rosso); (3,Verde); (4,Verde);
(6,Verde); (7,Rosso)];;


let getcolor c lst = 
	try List.assoc c lst 
	with _ -> Neutro;;

let rec vicini n = function 
[] -> []
| (a,b)::rest -> if a = n then b::vicini n rest
 							else if b=n then a::vicini n rest
							else vicini n rest;;


type 'a graph = ('a * 'a) list;;

let grafo = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;

exception NotFound;;

let path g colors lst start = 
	let rec from_node visited nodo lst = 
		if List.mem nodo visited then raise NotFound
		else 
			let nodocolore = getcolor nodo colors in
				if  lst = [nodocolore] then [nodo]
				else if lst=[] then raise NotFound
				else 
					let newlist = if List.hd lst = nodocolore then lst else List.tl lst in
					nodo::from_list (nodo::visited) newlist (vicini nodo g)
			and from_list visited lst = function
				| [] -> raise NotFound
				| x::rest -> try from_node visited x lst with _ -> from_list visited lst rest
			in from_node [] start lst;;  		

path grafo cols [Rosso;Verde;Neutro] 1;; 