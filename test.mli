type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
val albero : int tree
exception NotFound
val root : 'a tree -> 'a
