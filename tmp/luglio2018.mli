type 'a ntree = Tr of 'a * 'a ntree list
val ntree : int ntree
exception NotFound
val depth : 'a ntree -> 'a -> int
val depth_rec : 'a ntree -> 'a -> int
val appartiene : 'a ntree -> 'a -> bool
val parentela : 'a ntree -> 'a -> 'a -> int
val from_list : 'a -> 'a -> 'a ntree list -> int
