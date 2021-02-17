type form = True 
| False 
| Prop of string 
| Not of form 
| And of form * form 
| Or of form * form 
| Imp of form * form;;

let rec complessita = function 
	| Not f -> 1 + complessita f
	| And(f,g) | Or(f,g) | Imp(f,g) -> 1 + complessita f + complessita g
	| _ -> 0;;

let rec mkand = function 
	| [] -> True
	| [f] -> f 
	| f::rest -> And(f, mkand rest);;

let rec mkor = function 
	| [] -> False 
	| [f] -> f
	| f::rest -> Or(f, mkor rest);;

let rec complementare = function
	True | Not False -> False
	| False | Not True -> True
	| Prop p -> Not(Prop p)
	| Not(Prop p) -> Prop p
	| _ -> failwith "complementare";;


let rec test_nnf = function 
	| Prop _ | Not(Prop _) | True | False | Not True | Not False -> true
	| And(f,g) | Or(f,g) -> test_nnf f && test_nnf g
	| _ -> false

let rec duale = function 
	| And(f,g) -> Or(duale f, duale g)
	| Or(f,g) -> And(duale f, duale g)
	| f -> try complementare f with _ -> failwith "duale";;

let literal = function 
	| True | False | Not True | Not False | Prop _ | Not(Prop _) -> true
	| _ -> false;;

let rec and2list = function 
	| And(f,g) -> (and2list f)@(and2list g)
	| f -> if literal f then [f] else failwith "and2list";;


 