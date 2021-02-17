(*------------- Gruppo11 -------------*)
(* --- Esercizi della seconda parte --*)

type form =
    True
  | False
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
  | Imp of form * form


(*============ Es1:complessita =============*)

(*complessita : form -> int *)
let rec complessita = function
    Not f -> 1 + complessita f
  | And(f,g) | Or(f,g) | Imp(f,g) -> 1 + complessita f + complessita g
  | _ -> 0
	
(*============ Es2:mkand =============*)

(* mkand : form list -> form *)
let rec mkand = function
    []-> True
  | [f]-> f
  | f::rest -> And(f, mkand rest)

(*============ Es3:mkor =============*)

(* mkor : form list -> form *)
let rec mkor = function
    []-> False
  | [f]-> f
  | f::rest -> Or(f, mkor rest)

(*============ Es4:complementare =============*)

(* complementare: form -> form *)
let complementare = function
    True | Not False -> False
  | False | Not True -> True
  | Prop p-> Not(Prop p)
  | Not(Prop p)-> Prop p
  | _ -> failwith "complementare"


(*============ Es5:test-nnf =============*)

(* test_nnf : form -> bool *)
let rec test_nnf = function
    Prop _ | Not(Prop _) | True | False | Not True | Not False -> true
  | And (f,g) | Or(f,g) -> test_nnf f & test_nnf g
  | _ -> false


(*============ Es6:duale =============*)

(* duale: form -> form *)
let rec duale = function
      And(f,g) -> Or(duale f,duale g)
    | Or(f,g) -> And(duale f,duale g)
    | f -> (* se f e' un letterale ne riporta il complementare *)
	try complementare f 
	with Failure  "complementare" -> failwith "duale"

(*============ Es7:and2list =============*)

(* funzione ausiliaria che verifica se una formula e' un letterale *)
(* literal: form -> bool *)
let literal = function
    True | False | Not True | Not False 
  | Prop _ | Not(Prop _) -> true
  | _ -> false

(* and2list : form -> form list *)
let rec and2list = function 
    And(f,g) -> (and2list f)@(and2list g)
  | f -> if literal f then [f]
	 else failwith "and2list"

(*============ Es8:satxand-of-lits =============*)

(* satxand_of_lits: form -> bool *)
let satxand_of_lits f =
  (* aux: form list -> bool
     aux flist riporta true se nessun elemento di flist
         ha il proprio complemento in flist stessa,
         e  flist non contiene ne' False ne' Not True.
         Non val la pena utilizzare List.for_all, dato
         che la ricerca di un complementare puo' partire
         dall'elemento successivo *)
  let rec aux = function
      [] -> true
    | False::_ | (Not True)::_  -> false
    | f::rest -> not (List.mem (complementare f) rest) && aux rest
  in aux (and2list f)

(*============ Es9:int2form =============*)

type interpretation = (string * bool) list

(* funzione ausiliaria
   pair2lit: (string * bool) -> form
      riporta il letterale che "rappresenta" la coppia *)
let pair2lit = function
    (p,true) -> Prop p
  | (p,false) -> Not(Prop p)

(* int2form: interpretation -> form *)
let int2form emme =
  mkand (List.map pair2lit emme)

(*============ Es10:dnf =============*)
(* alla fine a lezione non abbiamo piu' visto il codice per trasformare
   una formula in forma normale congiuntiva,
   quindi questo esercizio non e' banale *)

(* puo' essere utile avere anche una funzione che controlli
   se una formula e' in dnf oppure no. *)

(* literal : form -> bool
   literal f = true se f e' un letterale *)
let literal = function 
    True | Not True | False | Not False |
    Prop _ | Not (Prop _) -> true
  | _ -> false

(* conjoflits : form -> bool
   conjoflits f = true se f e' una congiunzione di letterali *)
let rec conjoflits f =
  literal f || 
    match f with
      And(f,g) ->  conjoflits f && conjoflits g
    | _ -> false

(*   check_dnf: form -> bool *)
(* check_dnf f = true se f e' in forma normale disgiuntiva *)
let rec check_dnf = function
    Or(f,g) -> check_dnf f && check_dnf g
  | f -> conjoflits f

(* -------------------------------------------------- *)
(* la prima soluzione che propongo utilizza un algoritmo simile a 
   quello per la trasformazione in CNF non visto a lezione *)
(* Notate che la versione che era in rete sui lucidi proiettati a
   lezione fino a qualche tempo fa in realta' non era corretta.
   Quindi controllate la versione che avete scaricato. *)

(* funzioni ausiliaria per la trasformazione in forma normale negativa *)
(* fnn: form -> form *)
let rec fnn = function
  | And(f,g) -> And(fnn f,fnn g)
  | Or(f,g) -> Or(fnn f,fnn g)
  | Imp(f,g) -> fnn(Or(Not f,g))
  | Not(And(f,g)) -> Or(fnn(Not f),fnn(Not g))
  | Not(Or(f,g))  -> And(fnn(Not f),fnn(Not g))
  | Not(Imp(f,g)) -> And(fnn f,fnn(Not g))
  | Not(Not f) -> fnn f
  | f -> f


(* dnf : form -> form *)
let rec dnf f = 
  let rec distrib  = function
      (* applicazione delle distributive *)
      And(f,g) ->
	(match (distrib f,distrib g) with 
	  (Or(f1,g1),h) | (h,Or(f1,g1)) -> 
	    distrib (Or(And(h,f1), And(h,g1)))
	| (f1,h) -> And(f1,h))
    | Or(f,g) -> Or(distrib f, distrib g)
    | f -> f
  in distrib(fnn f);;

(* ------------------------------------ *)    
(* Algoritmo alternativo: partire dalla tabella di verita'
   della formula, e costruire l'OR della "rappresentazione" 
   di ciascuna interpretazione in cui la formula e' vera *)
(* rappresentiamo  le interpretazioni come nell'esercizio 9 *)
type interpretation = (string * bool) list
(* funzioni ausiliarie viste a lezione, modificate per tener
   conto di questo modo diverso di rappresentare le interpretazioni *)

(* models: form -> interpretation -> bool *)
let rec models f emme = 
  match f with
    True -> true
  | False -> false
  | Prop name -> List.mem (name,true) emme
  | Not f1 -> not(models f1 emme)
  | And(f1,f2) -> models f1 emme & models f2 emme 
  | Or(f1,f2) -> models f1 emme or models f2 emme 
  | Imp(f1,f2) -> not(models f1 emme) or models f2 emme

(* setadd : 'a -> 'a list -> 'a list *)
let setadd x xs 
    = if List.mem x xs then xs else x::xs

(* union : 'a list -> 'a list -> 'a list *)
let rec union lst ys =
  match lst with 
    [] -> ys
  | x::xs -> setadd x (union xs ys);;
        
(* atomlist : form -> string list *)
let rec atomlist = function
    True | False -> []
  | Prop s -> [s]
  | Not f -> atomlist f
  | Or(f1,f2) -> union (atomlist f1) (atomlist f2)
  | And(f1,f2) -> union (atomlist f1) (atomlist f2)
  | Imp(f1,f2) -> union (atomlist f1) (atomlist f2)

(* cons : 'a -> 'a list -> 'a list *)
let cons x xs = x::xs;;

(* generazione di tutte le interpretazioni di una lista
   di atomi *)
(* all_ints : 'a list -> ('a * bool) list list *)   
let rec all_ints = function
    [] -> [[]]
  | p::props -> 
       let result = all_ints props
       in (List.map (cons (p,true)) result) @ 
           (List.map (cons (p,false)) result)

(* truthtable : form -> (interpretation * bool) list *)
let truthtable f =
  let mkrow f emme = (emme, models f emme)
  in List.map (mkrow f)(all_ints (atomlist f));;

(* funzione che colleziona le interpretazioni in cui una formula
   e' vera *)
(* all_models : form -> (string * bool) list list *)
let all_models f =
  List.map fst
    (List.filter (fun (emme,b) -> b) (truthtable f))

(* dnf: form -> form *)
let dnf f =
  mkor (List.map int2form (all_models f))

(* --------------------------- *)
(* Ancora un altro algoritmo e' stato visto a lezione, ed 
   utilizza i tableaux (vedere la funzione dnf_tab nel file 
   logic.ml, riscritta anche in fondo all'esercizio seguente) *)

(*============ Es11:tab-cnf =============*)

(* riporto per completezza il codice visto a lezione per raccogliere
   tutti i rami aperti di un tableau completo *)
(* alpha : form -> form * form *)
let alpha = function
    And(f,g) -> (f,g)
  | Not(Or(f,g)) -> (Not f,Not g)
  | Not(Imp(f,g)) -> (f,Not g)
  | _ -> failwith "alpha"

(* val beta : form -> form * form *)
let beta = function
    Or(f,g) -> (f,g)
  | Not(And(f,g)) -> (Not f,Not g)
  | Imp(f,g) -> (Not f,g)
  | _ -> failwith "beta"

(* complement : form -> form *)
let complement = function
    Prop p -> Not(Prop p)
  | Not(Prop p) -> Prop p
  | _ -> failwith "complement"

(* all_models: form list -> form list list *)
let all_models formlist =
  let rec aux pending lits = 
    match pending with
      [] -> [lits]
    | f::rest ->
        match f with
          True | Not False -> aux rest lits
        | False | Not True -> []
        | Prop _ | Not(Prop _) ->
            if List.mem f lits then aux rest lits
            else if List.mem (complement f) lits  then []
            else aux rest (f::lits)
        | Not(Not f) -> aux (f::rest) lits
	| And(_,_) | Not(Or(_,_)) | Not(Imp(_,_)) ->
	    let (f1,f2)= alpha f in
	    aux (f1::f2::rest) lits
	| Or(_,_) | Not(And(_,_)) | Imp(_,_) -> 
	    let (f1,f2) = beta f in
            (aux (f1::rest) lits) @ (aux (f2::rest) lits)
   in aux formlist []

(* l'algoritmo per trasformare una formula f in CNF raccoglie
   i rami aperti in un tableau completo per Not f, 
   ne complementa i letterali, li mette in OR, e infine in 
   And le disgiunzioni cosi' ottenute.
   A voi il compito di riscrivere il codice senza tanti map! *)
(* cnf_tab : form -> form *)
let cnf_tab f =
  mkand 
    (List.map mkor 
       (List.map (List.map complement) (all_models [Not f])))

(* in alternativa, si puo' utilizzare la funzione duale e la funzione
   dnf_tab vista a lezione, qui riscritta in maniera piu' compatta *)
(* dnf_tab: form -> form *)
let dnf_tab f =
  mkor (List.map mkand  (all_models [f]))

let cnf_tab f =
  duale (dnf_tab (Not f))

(*============ Es12:logical-consequence =============*)

(* si puo' utilizzare la funzione sat o sat_tab.
   Utilizzando la sat_tab vista a lezione, possiamo definire:

let logical_consequence flist f =
  try let _ = sat_tab ((Not f)::flist) 
      in false
  with NotSat -> true

   Oppure possiamo ridefinire sat_tab,  in modo che
   riporti un booleano e non una lista di letterali.
   Le funzioni ausiliarie usate sono state definite prima. *)

(* form list -> bool *)
let sat_tab formlist =
  let rec aux pending lits = 
    match pending with
      [] -> true
    | f::rest ->
        match f with
          True | Not False -> aux rest lits
        | False | Not True -> false
        | Prop _ | Not(Prop _) ->
            if List.mem f lits then aux rest lits
            else not(List.mem (complement f) lits)
                  && aux rest (f::lits)
        | Not(Not f) -> aux (f::rest) lits
	| And(_,_) | Not(Or(_,_)) | Not(Imp(_,_)) ->
	    let (f1,f2)= alpha f in
	    aux (f1::f2::rest) lits
	| Or(_,_) | Not(And(_,_)) | Imp(_,_) -> 
	    let (f1,f2) = beta f in
            aux (f1::rest) lits ||
	         aux (f2::rest) lits
   in aux formlist []

(* logical_consequence: form list -> form _> bool *)
let logical_consequence flist f =
  not (sat_tab ((Not f)::flist))

(* se si vuole utilizzare la funzione sat vista a lezione, 
   il cui tipo e'    sat : form -> bool 
       let sat f =
          List.exists (models f) (powset (atomlist f))
   si deve costruire la congiunzione di Not f con la congiunzione
   di tutte le formule in flist:

let logical_consequence flist f =
    not (sat (mkand ((Not f)::flist)))
*)