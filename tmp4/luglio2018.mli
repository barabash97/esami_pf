type 'a ntree = Tr of 'a * 'a ntree list
val ntree : int ntree
exception NotFound
val depth : 'a -> 'a ntree -> int
val appartiene : 'a -> 'a ntree -> bool
val parentela_b : 'a -> 'a -> 'a ntree -> int
val parentela : 'a ntree -> 'a -> 'a -> int
val from_list : 'a -> 'a -> 'a ntree list -> int
