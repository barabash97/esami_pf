type 'a tree_bin = Empty | Tr of 'a * 'a tree_bin * 'a tree_bin
val albero_bin : int tree_bin
exception NotFound
val n_ramo_bin : int -> int tree_bin -> int list
type 'a ntree = Tr of 'a * 'a ntree list
val ntree : int ntree
val n_ramo : int -> int ntree -> int list
