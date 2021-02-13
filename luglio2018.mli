type 'a ntree = Tr of 'a * 'a ntree list
val ntree : int ntree
val depth : int -> 'a -> int
val parentelaOld : 'a -> int -> int -> int
val parentela : int -> int -> 'a ntree -> int
val sc : int -> int -> 'a ntree list -> int
