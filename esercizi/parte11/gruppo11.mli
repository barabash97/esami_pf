type form =
    True
  | False
  | Prop of string
  | Not of form
  | And of form * form
  | Or of form * form
  | Imp of form * form
val complessita : form -> int
val mkand : form list -> form
val mkor : form list -> form
val complementare : form -> form
val test_nnf : form -> bool
val duale : form -> form
val literal : form -> bool
val and2list : form -> form list
