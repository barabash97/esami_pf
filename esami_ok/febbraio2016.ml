type 'a action_graph = ('a * string * 'a) list;;

let grafo = [(1,"a",2); (1,"b",3); (1,"c",4); (2,"a",6); (3,"b",5);
(3,"c",5); (4,"b",1); (4,"c",6); (5,"c",4); (5,"a",5);
(5,"b",5); (6,"b",5)];;


(* successori: 'a -> 'a action_graph -> (string * 'a) list *)
let rec successori x = function 
	[] -> []
	| (a,v,b)::rest -> if a = x then (v,b)::successori x rest
										else successori x rest;;

successori 5 grafo;;

(* addset: 'a -> 'a list -> 'a list *)
let rec addset x lst =
	if List.mem x lst then lst else x::lst;;

(* first_element: 'a action_graph -> 'a *)
let rec first_element = function 
	 (a,v,b)::rest -> a;;

(* nodes: 'a action_graph -> 'a list *)
let  nodes g = 
	let rec aux result = function 
		| [] -> result
		| (a,v,b)::rest -> aux (addset b result)  rest
	in aux [] g;;	
nodes grafo;;

(* check: 'a action_graph -> bool *)
let check g = 
	let rec aux result flag = function 
		| [] -> flag
		| (a,v,b)::rest -> 
									if (List.mem v result) then false else aux (v::result) (true) rest
									
	in aux [] false g;;								


let g2vero = [(1,"a",2); (1,"b",3)];;

let g2false = [(1,"a",2); (1,"a",3)];;


check g2vero;;
check g2false;;

(* getelem: 'a -> 'a -> 'a option_graph -> string *)
let rec getelem a b = function 
	[] -> []
	|(x,e,y)::rest -> if x = a && b = y then [e] else getelem a b rest;;  

getelem 1 2 grafo;;


(* move: 'a action_graph -> 'a -> 'a -> string list *)
let move g start goal = 
	let rec from_node visited nodo =  
		if List.mem nodo visited then failwith "from_node"
		else 
			from_list (nodo::visited) nodo (successori nodo g)
		and from_list visited nodo = function 
			| [] -> []
			| (e,a)::rest -> if a = goal then [e] else try e::from_node visited a with _ -> from_list visited a rest
		in from_node [] start;; 	

move grafo 3 6;;

