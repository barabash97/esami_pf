type 'a ntree = Tr of 'a * 'a ntree list;;

let ntree = Tr(1, 
						[
							Tr(2, 
									[
										Tr(5, []); 
										Tr(8, [Tr(9, [Tr(15, [])]);
										Tr(10, [])]) ]); 
										Tr(3, [Tr(6, []); Tr(7, []); Tr(18, [Tr(29,[]);Tr(4, [])])]);
										Tr(14, [Tr(19,[]); Tr(12, []); Tr(29, [Tr(13, [])])])
										
										]);;

