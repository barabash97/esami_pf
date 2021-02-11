type  color = Rosso | Verde | Neutro;;

type 'a cols = ('a * color) list;;

let cols = [(2,Rosso); (3,Verde); (4,Verde);
(6,Verde); (7,Rosso)];;

let nodes =  [1;2;3;4;5;6;7;8;9;10];;

let trova_colore e lst = 
	try 
		List.assoc e lst
	with _ -> Neutro;;
			 		
let rec incrementaset c = function 
	| [] -> [(c, 1)]
	| (a,b)::rest -> if a = c then (a, b+1)::rest else (a,b)::incrementaset c rest;;

incrementaset Neutro [];;

(* conta_colori: 'a cols -> 'a list -> ('a color * 'a) list *)
let  conta_colori cols nodes = 
	let rec aux result = function 
		| [] -> result
		| x::rest -> aux (incrementaset (trova_colore x cols) result) rest
	in aux [] nodes;;

conta_colori cols nodes;;				

(* ESERCIZIO 2 *)

type 'a graph = ('a * 'a) list;;

let g = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;
	
let nodeColors = [(1,Neutro); (2,Rosso); (3,Verde); (4,Verde); (5,Neutro); (6, Verde); (7,Rosso); (8,Neutro)];;

let rec successori n = function 
	| [] -> []
	| (a,b)::rest -> if a = n then b::successori n rest
									 else if b=n then a::successori n rest
									else successori n rest;;

let nuova_lista_colori colore lst =
	if (List.hd lst) = colore then lst else List.tl lst;;  
 
let path g nodeColors lstColors start = 
	
	let rec from_node visited nodo lst =  
		
		if List.mem nodo visited then failwith "from_node"
		
		else let node_color = trova_colore nodo nodeColors in 
				if lst = [] || lst = [node_color] then [nodo]
				else 
					let new_list = if List.hd lst = node_color then List.tl lst else lst 
					in nodo::from_list (nodo::visited) nodo new_list (successori nodo g)
		and from_list visited nodo lst = function 
			| [] -> failwith "from_list"
			| x::rest -> 
							try 
								from_node visited x lst 
							with _ -> from_list visited x lst rest
	in from_node [] start lstColors;;

path g nodeColors [Rosso;Verde;Neutro] 1;;																 				
					
					 
				  
					  	
																																																																