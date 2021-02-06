type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree;;

let albero = Tr(5, Tr(7, Tr(3, Empty,Empty), Tr(9, Empty,Empty)), Tr(1, Tr(4,Empty,Empty), Tr(5,Empty,Empty)));;
exception NotFound;;

let root = function
	Empty -> raise NotFound
	|Tr(x,_,_) -> x;;
