(* ESERCIZIO 1*)

type color =  Verde | Rosso | Neutro;;

type 'a lst = ('a * color) list;;

let lista = [1;2;3;4;5;6;7;8;9;10];;

let cols = [(2,Rosso); (3,Verde); (4,Verde);
(6,Verde); (7,Rosso)];;

let rec incrementa colore = function 
	[] -> [(colore, 1)]
	| (c,numero)::rest -> if c = colore then (c,numero+1)::rest else (c,numero)::incrementa colore rest;;

let rec prendi_colore_elemento e = function 
	 [] -> Neutro 
	| (a,b)::rest -> if e=a then b else prendi_colore_elemento e rest;;

let conta_colori color lista = 
	let rec aux result color = function 
		[] -> result
		| n::rest -> aux (incrementa (prendi_colore_elemento n color) result) color rest
	in aux [] color lista;;

conta_colori cols lista;; 

(* ESERCIZIO 1 OK*)

(* ESERCIZIO 2*)

let listaColoriGrafo = [(1,Neutro); (2,Rosso); (3,Verde); (4,Verde); (5,Neutro); (6, Verde); (7,Rosso); (8,Neutro)];;

type 'a graph = ('a * 'a) list;;

let grafo = [(1,3);(1,2); (3,2); (2,3); (3,4); (3,5); (2,5); (4,5); (5,6); (5,7); (6,7); (7,8)];;

let rec successori n = function 
[] -> []
| (a,b)::rest -> if a = n then b::successori n rest
 							else if b=n then a::successori n rest
							else successori n rest;;

let get_color_node node colors = 
	try 
		List.assoc node colors
	with Not_found -> Neutro;;	


let path g colors lst start =
	let rec from_node node visited lst = 
		
		if List.mem node visited then failwith "path"
		
		else let node_color = get_color_node node colors in
			if lst = [] || lst = [node_color] then [node]
			else 
				let new_list = if List.hd lst = node_color then List.tl lst else lst
				 in node::from_list (node::visited) new_list (successori node g)
		
			and from_list visited list = function
      [] -> failwith "path"
    | n::rest ->
        try from_node n visited list
        with _ -> from_list visited list rest
  in from_node start [] lst;;			


path grafo listaColoriGrafo [Rosso;Verde;Neutro] 1;;








