type 'a metro = (int * int * string) list;;

let metro = [(1,2,"A"); (2,3,"A");
(3,1,"A"); (2,4,"B"); (4,5,"B"); (4,6,"C"); (6,3,"C"); (5,7,"D");
(6,7,"D")];;

let addset l lst = 
	if List.mem l lst then lst else l::lst;;

let line n m = 
	let rec aux result = function 
		| [] -> result
		| (a,b,c)::rest -> if a = n || b = n then aux (addset c result) rest else aux result rest

	in aux [] m;;

line 5 metro;;

let rec vicini n = function
	[] -> []
	|(a,b,c)::rest -> if a = n then (b,c)::vicini n rest else if b=n then (a,c)::vicini n rest else vicini n rest;;

vicini 1 metro;;

let raggiungi m maxc start goal = 
	let rec from_node visited nodo cambi predln = 
		if List.mem nodo visited then failwith "from_node"
		else if cambi > maxc then failwith "from_node" else 
			if nodo = goal then [nodo]
			else nodo::from_list (nodo::visited) cambi predln (vicini nodo m)
	
			and from_list visited cambi line = function
				| [] -> failwith "from_list"
				| (n,l)::rest -> let new_cambi = if line <> l then (cambi+1) else cambi in 
													try from_node visited n new_cambi l with _ -> from_list visited new_cambi l rest
													
			in from_node [] start (-1) "";;						

raggiungi metro 2 6 1;;										   
					