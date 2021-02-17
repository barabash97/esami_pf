type 'a ntree = Tr of 'a * 'a ntree list
val ntree : int ntree
val depth : int -> 'a -> int
val appartiene : 'a ntree -> 'a -> bool
exception NotFound
val profondita : 'a ntree -> 'a -> int
val parentela : 'a ntree -> 'a -> 'a -> int
val from_list : 'a -> 'a -> 'a ntree list -> int
