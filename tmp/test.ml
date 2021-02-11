type 'a graph = ( 'a * 'a ) list
type 'a option = Some of 'a  | None
(** il tipo 'a option e' predefinito, non e' necessario ridefinirlo **)

let rec successori n = function
    [] -> []
  | (x,y)::rest -> if x=n then y::successori n rest
  else  successori n rest;;

let grafo = [(1,3);(1,2);(3,4);(4,3);(4,2);(2,5);(5,5)]

(* checks: 'a -> 'a option -> bool 
   checks n option  = true se option = None oppure in caso di Some(y)
                       allora y=n, false se y<>n *)     
let checks n = function
    Some(y) -> y=n
  | None -> true

(* checkuno: 'a list -> bool
   checkuno lista = true con lista = [] altrimenti false *)
	
let checkuno =function
    [x] -> true
  | _ -> false

(** mi sembra che la specifica non corrisponda al codice:
    checkuno list = true se list contiene uno ed un unico elemento **)

(* aux: 'a list -> 'a option list -> 'a list -> 'a list -> 'a list
 aux visited l path successori = path  di nodi (se esiste) che vanno da start a goal e
 che  fino al k-esimo elemento di l soddisfano i pattern di l e possono avere
   ripetizioni, dopo il k-esimo elemento non devono avere ripetizioni *)

(** parli dello start della funzione whichpath? E qual e' il ruolo di
    visited, path e successori? **)

(* cammino: 'a list -> 'a list -> 'a list -> 'a -> 'a graph -> 'a list 
   cammino visited path goal g successori = path senza cicli fino a goal, se
   esiste altrimenti fail *)
(** anche qui, path che parte da dove? e qual e' il ruolo degli altri 
    parametri? **)
	
let whichpath g plist start goal =
  let rec aux visited l path = function
      [] -> failwith "q"
    | n::rest -> match l with
	[] -> cammino visited path goal g (n::rest)
      | _->             

	  if goal = n && checkuno l &&  checks n (List.hd l) then List.rev (n::path)
	  else if  checks n (List.hd l)
	  then try aux (n::visited) (List.tl l) (n::path) (successori n g)
	  with _ -> aux visited l path rest
	  else aux visited l path rest
  and   cammino visited path goal g = function
      [] -> failwith "q"
    | a::rest -> if a = goal then List.rev (a::path)
    else  if List.mem a visited then cammino visited path goal g   rest
    else try cammino (a::visited) (a::path) goal g (successori a g)
    with _ -> cammino visited path goal g rest
  in aux [] plist [] [start]

(** e' inutile passare a cammino g e goal come parametri: non cambiano mai
    E in generale, path e visited sono sempre la stessa lista: basta una
    delle due **)
		
whichpath g [None;None;Some 3] 1 4;;		