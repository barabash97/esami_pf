type 'a option = None | Some of 'a;;

type 'a graph = ('a * 'a) list;;

let g = [(1,3);(1,2);  (3,4); (4,3); (4,2); (2,5); (5,5)];;

(* vicini: 'a -> 'a graph -> 'a list *)
let rec vicini x = function 
	| [] -> []
	| (a,b)::rest -> if a = x then b::vicini x rest
									else vicini x rest;;

(* checkpattern: 'a -> 'a option -> bool *)
let checkpattern x = function 
	| None -> true
	| Some y -> x = y;;


(* let listlength = List.length patterns in
			
				if listlength = 0 then false (* controllo aciclico *)
				else   *)

				
								(* NON FUNZIONA *)				
exception NotFound;;				
let whichpath g lista start goal = 
	let rec from_node visited nodo patterns = 	
			if List.mem nodo visited  then failwith "from_node" else
			let flagcheck = checkpattern nodo (List.hd patterns) in
						if flagcheck = false then raise NotFound else
					 if nodo = goal && flagcheck then [nodo]
						else 
							if not(flagcheck) then failwith "from_node" else 
							nodo::from_list (nodo::visited) nodo (List.tl patterns) (vicini nodo g) 
		
				and from_list visited nodo patterns = function 
				| [] -> []
				| x::rest -> try	
											from_node visited x patterns 
											with _ -> from_list visited x patterns rest
											
											
	in from_node [] start lista;;										

whichpath g [None;None;Some 3] 1 4;;
