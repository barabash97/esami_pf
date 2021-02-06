type 'a metro = ('a * 'a  * 'a) list;;

let m = [(1,2,"A"); (2,3,"A");
(3,1,"A"); (2,4,"B"); (4,5,"B"); (4,6,"C"); (6,3,"C"); (5,7,"D");
(6,7,"D")];;

let addset n lst = 
	if List.mem n lst then lst else n::lst;;

let  line m ln = 
	let rec aux result = function 
		[] -> result 
		|(a,b,l)::rest -> if l = ln then aux (addset a (addset b result)) rest else aux result rest
	in aux [] m;;

 line m "A";;

let rec vicini nodo = function 
	[] -> []
	|(a,b,l)::rest -> if nodo = a then (b,l)::vicini nodo rest  
	else if nodo = b then (a,l)::vicini nodo rest
	else vicini nodo rest;;

vicini 4 m;;

exception NotFound;;

(* raggiungi: metro -> int -> int -> int -> int list *)
let raggiungi m maxc start goal =
  (* from_node: int -> string -> int -> int list -> int list 
   *            A partire da una stazione (node) avente linea precedente (linea)
   *            e cambi disponibili trova un percorso dalla stazione corrente
   *            fino al nodo goal senza passare per le stazioni contenute
   *            nella lista visited. Fallisce altrimenti.
   *)
  let rec from_node node linea cambi visited = 
    if List.mem node visited || cambi > maxc
    then failwith "raggiungi"
    else if node = goal
    then [node]
    else node :: from_list linea cambi (node::visited) (vicini node m)
  (* from_list: string -> int -> int list -> (int * string) list -> int list
   *            Trova un cammino a partire dalla lista di coppie stazioni, linea
   *            fino ad un nodo goal cambiando massimo "cambi" volte linea e 
   *            considerando "linea" come linea precedente alle stazioni della
   *            lista. Fallisce altrimenti.
   *)
  and from_list linea cambi visited = function
      [] -> failwith "raggiungi"
    | (n,ln)::rest ->
        try 
          let new_cambi = if linea <> ln then cambi + 1 else cambi 
          in from_node n ln new_cambi visited
        with _ -> from_list linea cambi visited rest        
  in from_node start "" (-1) [];;

raggiungi m 1 6 1;;

(* FINE ESAME: OK*)