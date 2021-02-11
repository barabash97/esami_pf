(* ESERCIZIO 1 *)

let output = [1;2;3;0;5;4;8;7;10;9];;

let rec elementi_maggiori value = function 
	| [] -> []
	| x::rest -> if x > value then x::elementi_maggiori x rest else elementi_maggiori value rest;; 

let rec ordinati start = function 
	| [] -> []
	| x::rest -> if x = start then x::elementi_maggiori x rest else ordinati start rest;;

ordinati 3 output;;

(* ESERCIZIO 1 - OK *)

type 'a ntree = Tr of 'a * 'a ntree list;;

let leaf x = Tr(x,[]);;

let t = Tr(1,[Tr(2,[Tr(3,[leaf 4;
 leaf 5]);
 Tr(6,[leaf 7]);
 leaf 8]);
 leaf 9;
 Tr(10,[Tr(11,[leaf 12;
 leaf 13;
 leaf 14]);
 leaf 15;
 Tr(16,[leaf 17;
 Tr(18,[leaf 19;
 leaf 20])])])])



let rec livello_b k (Tr(x,tlist)) =
 if k=0 then [x]
 else List.flatten (List.map (livello_b (k-1)) tlist);;

	livello_b 3 t;;			

let rec livello k (Tr(x, tlist)) = 
	if k = 0 then [x]
	else x::from_list (k-1) tlist 
	and from_list k = function 
		| [] -> []
		| x::rest -> (livello k x)@(from_list k rest);;					

livello_b 3 t;;				

(* ESERCIZIO 2 - OK *)

(* ESERCIZIO 3 *)  																									 		 					 						

type 'a graph = ('a * 'a) list;;

let rec vicini x = function 
	| [] -> []
	| (a,b)::rest -> if x=a then b::vicini x rest
									else if  x=b then a::vicini x rest
									else vicini x rest;; 

let path g k p start = 
	let rec from_node visited nodo count = 
		if List.mem nodo visited || count > k then failwith "from_node"
		else 
			

(* END ESERCIZIO 3 *)