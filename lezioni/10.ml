type 'a graph = ('a * 'a) list
let grafo = [(1,2); (1,3); (1,4); (2,6); (3,5); (4,6);
             (5,4); (6,5); (6,7)];;

type 'a ntree = Tr of 'a * 'a ntree list;;

let rec successori nodo = function 
      [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest;;

let rec vicini nodo = function 
    [] -> []
  | (x,y)::rest ->
      if x = nodo then y::vicini nodo rest
      else if y = nodo then x::vicini nodo rest
      else vicini nodo rest;;

let setadd x set = 
  if List.mem x set then set else x::set;;

let rec nodes = function
    [] -> []
  | (x,y)::rest ->
      setadd x (setadd y (nodes rest));;

(* ALGORITMI BASE GRAFI *)

(* VISITA IN PROFONDITA (grafo orientato) *)

let depth_first_collect graph start = 
	let rec search visited = function 
		| [] -> visited
		| n::rest -> if List.mem n visited then search visited rest
									else search (n::visited) ((successori n graph)@rest)
									
	in search [] [start];;

(* VISITA IN PROFONDITà CON UNA CONDIZIONE *)
let depth_first_all graph start p = 
	let rec search visited p = function 
		[] -> true 
		|n::rest -> if List.mem n visited then search visited p rest
								else n p && search (n::visited) p ((successori n graph)@rest)
	in search [] p [start];;		

(* VISITA IN AMPIEZZA *)

let breadth_first_collect graph start = 
	let rec search visited = function
	 []				  -> visited
		| n::rest -> if List.mem n visited then search visited rest
									else search (n::visited) (rest@(successori n graph))
	in search [] [start];;

(* VISITA ALBERO IN PREORDINE *)

let rec preorder (Tr(x,tlist)) =
  x::preorder_tlist tlist
and preorder_tlist = function
  [] -> []
| t::ts -> preorder t @ preorder_tlist ts;;

(* max: int -> int -> int *)
let max x y = if x > y then x else y;;

(* elemento massimo della lista *)

let rec maxl = function 
	[x] -> x
	|x::rest -> max x (maxl rest)
	| _ -> failwith "maxl";;

(* altezza albero con le funzioni delle liste *)
let rec height (Tr(x,tlist)) = match tlist with
[] -> 0
| _ ->  1 + maxl (List.map height tlist);;

(* altezza albero con mutua ricorsione *)
let rec h (Tr(x,tlist)) = match tlist with
[] -> 0
| _ -> 1+ hl tlist
and hl = function
  [] -> failwith "h"
| [t] -> h t
| t::rest -> max (h t) (hl rest);;

(* Test esistenza nodo nell'albero *)

let rec occurs_in (Tr(x,tlist)) y = 
  x=y || occurs_in_tlist tlist y
and occurs_in_tlist tlist y = match tlist with
[] -> false
| t::ts -> occurs_in t y || occurs_in_tlist ts y;;

(* Verificare che un nodo è discendente di un altro nodo*)

let rec descend (Tr(x, tlist)) y z = 
	if x=y then occurs_in_tlist tlist z
	else descend_tlist tlist y z 
	and descend_tlist tlist y z = match tlist with
	| [] -> false
	| t::rest -> descend t y z || descend_tlist rest y z;;

exception NotFound;;

(* cammino fino alla foglia etichettata da y in t *)
let rec path (Tr(x, tlist)) y = match tlist with
	[] -> if x = y then [x] else raise NotFound
	| _ -> x::path_tlist tlist y
	and path_tlist tlist y = match tlist with
	| [] -> raise NotFound
	| x::rest -> try path x y with _ -> path_tlist rest y;;

(* VERSIONE ALTERNATIVA: cammino fino alla foglia etichettata da y in t *)

let path_alternativo t y =
	let rec aux = function 
		| [] -> raise NotFound
		| Tr(x, [])::rest -> if x = y then [x] else aux rest
		| Tr(x, tlist)::rest -> try x::aux tlist with _ -> aux rest
	in aux [t];;

 


 
			  
									 																 

