type 'a treebin = Empty | Tr of 'a * 'a treebin * 'a treebin
val albero : int treebin
val labels : 'a treebin -> 'a list
val discendenti : 'a -> 'a treebin -> 'a list
