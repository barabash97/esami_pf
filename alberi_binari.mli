type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val size : 'a tree -> int
val albero : int tree
val tree_exist : ('a -> bool) -> 'a tree -> bool
val raccogli : 'a tree -> 'a list
val foglie : 'a tree -> 'a list
val nodi_con_un_figlio : 'a tree -> 'a list list
exception NotFound
val path_to : 'a -> 'a tree -> 'a list
val add : 'a -> ('a * int) list -> ('a * int) list
val count : 'a tree -> ('a * int) list
