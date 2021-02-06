type 'a tree_bin = Empty | Tr of 'a * 'a tree_bin * 'a tree_bin
val albero_bin : int tree_bin
val somma_albero_bin : int tree_bin -> int
val root_tree : 'a list tree_bin -> 'a list
exception NotFound
val n_ramo_bin : int -> int tree_bin -> int list
type 'a ntree = Tr of 'a * 'a ntree list
val albero_n : int ntree
val n_ramo : int -> int ntree -> int list
val n_ramo_list : int -> int ntree list -> int list
