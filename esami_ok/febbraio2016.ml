type 'a action_graph = ('a * string * 'a) list;;

let grafo = [(1,"a",2); (1,"b",3); (1,"c",4); (2,"a",6); (3,"b",5);
(3,"c",5); (4,"b",1); (4,"c",6); (5,"c",4); (5,"a",5);
(5,"b",5); (6,"b",5)];;

let rec successori x = function
	| [] -> []
	| (a,e,b)::rest -> if a = x then (e,b)::successori x rest 
										
											else successori x rest;;

successori 5 grafo;; 

let addset x lst = 
	if List.mem x lst then lst else x::lst;;

let rec nodes g = 
	let rec aux result = function 
		| [] -> result
		| (a,e,b)::rest -> aux (addset a (addset b result)) rest
	in aux [] g;;

nodes grafo;; 

let check_unique_succ lst = 
	let rec aux result flag = function
		| [] -> flag
		| (e,n)::rest -> if List.mem e result then false else aux (e::result) true rest
	in aux [] (true) lst;;

let rec check grafo = 
	let rec aux flag = function
		| [] -> flag
		| (a,e,b)::rest -> aux ((check_unique_succ (successori a grafo)) && (check_unique_succ (successori b grafo))) rest
	in aux (true) grafo;; 

check grafo;;

let move g start goal = 
	let rec from_node visited nodo = 
		if List.mem nodo visited then failwith "from_node"
		else if start=goal then []
		else 
			from_list (nodo::visited) (successori nodo g)
			and from_list visited = function
				| [] -> failwith "from_list"
				| (e,n)::rest -> try if n = goal then [e] else  e::from_node visited n with _ -> from_list visited rest
		in from_list [] (successori start g);;
move grafo 3 6;;