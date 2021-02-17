type 'a graph = ('a * 'a) list;;
type 'a money = ('a * int) list;;

let grafo = [('A','C'); ('A', 'B'); ('A', 'D'); ('B', 'E'); ('C','E'); ('C','F'); ('D','F'); ('E','G'); ('F', 'G')];;

let wallet =
[('C',-7);('D',-15);('F',3);('G',-5)];;

(* getnodeoprice: 'a -> 'a money -> int *)
let getnodeprice n wallet =
	try List.assoc n wallet with Not_found -> 0;;  

(* successori: 'a -> 'a graph -> 'a list *)
let rec successori nodo = function 
  [] -> []
  | (x,y)::rest -> 
    if x = nodo then y::successori nodo rest
    else successori nodo rest;;

let safe_path g wallet start goal init = 
	let rec from_node visited nodo cmoney = 
		if List.assoc nodo visited then failwith "from_node"
		else let new_money = cmoney + (getnodeprice nodo wallet) in
				if new_money < 0 then failwith "from_node"
				else if nodo = goal then [nodo]
				else 
					nodo::from_list (nodo::visited) nodo new_money (successori nodo g)
			
				and from_list visited nodo cmoney = function
					| [] -> failwith "from_list"
					| x::rest -> try from_node visited x cmoney with _ -> from_list visited x cmoney rest
			in from_node [] start init;;	
	


(* safe_path g wallet start goal init  *)