module assignment01

/*
	Course I00032 Advanced Programming 2014
	Skeleton for assignment 1
	Pieter Koopman
*/

import StdEnv

/**************** Prelude: *******************************/
//	Example types
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)         
:: Rose a	= Rose a [Rose a]

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b

//	Generic type representations
:: RoseG a	:== PAIR a [Rose a]

// Conversions
fromRose :: (Rose a)	-> RoseG a
fromRose (Rose a l)		= PAIR a l

// Oerdering

::	Ordering = Smaller | Equal | Bigger

class (><) infix 4 a :: !a !a -> Ordering

instance >< Int where		// Standard ordering for Int
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Char where		// Standard ordering for Char
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< String where	// Standard lexicographical ordering
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Bool where		// False is smaller than True
	(><) False True  = Smaller
	(><) True  False = Bigger
	(><) _     _     = Equal
	
/**************** End Prelude *************************/

/**************** 1 *************************/

/*instance >< Color where		// RYB
	(><) Red Red 		= Equal
	(><) Red _ 			= Smaller
	(><) Yellow Yellow	= Equal
	(><) Yellow Blue	= Smaller
	(><) Blue Blue 		= Equal
	(><) _ _	 		= Bigger
	
instance >< (Tree a) | >< a where
	(><) Tip 			Tip 			= Equal
	(><) Tip 			_ 				= Smaller
	(><) _ 				Tip 			= Bigger
	(><) (Bin a x1 y1) (Bin b x2 y2) 	= case a >< b of
											Equal = case x1 >< x2 of
														Equal = y1 >< y2
														ord = ord
											ord = ord*/
											
instance >< (Rose a) | >< a where
	(><) (Rose a x) (Rose b y) = case a >< b of
									Equal = x >< y
									other = other
									
/*instance >< (a,b) | >< a & >< b where
	(><) (x1,y1) (x2,y2) = case x1 >< x2 of
							Equal = y1 >< y2
							other = other*/
							
instance >< [a] | >< a where
	(><) [] 	[] 		= Equal
	(><) [] 	_ 		= Smaller
	(><) _ 		[] 		= Bigger
	(><) [x:xs] [y:ys] 	= case x >< y of
							Equal = xs >< ys
							other = other
							
/**************** End 1 *************************/		

/**************** 2 *************************/						

// 2.1
:: ColorG 	:== EITHER UNIT (EITHER UNIT UNIT)
:: ListG a 	:== EITHER UNIT (PAIR a [a])

// 2.2
listToGen :: [a] 	-> ListG a
listToGen [] 		= LEFT UNIT
listToGen [x:xs] 	= RIGHT (PAIR x xs) 

// 2.3
// RIGHT (PAIR 1 [2,3]).
// Yes, that's also the value obtained by listToGen [1,2,3]

// 2.4
// No, the generic representation of all types is different

/**************** End 2 *************************/	

/**************** 3 *************************/

// 3.1
instance >< UNIT where
	(><) _ _ = Equal
	
instance >< (PAIR a b) | >< a & >< b where
	(><) (PAIR x1 y1) (PAIR x2 y2) = case x1 >< x2 of
										Equal = y1 >< y2
										other = other
							
instance >< (EITHER a b) | >< a & >< b where
	(><) (LEFT _) (RIGHT _) 	= Smaller
	(><) (RIGHT _) (LEFT _) 	= Bigger
	(><) (LEFT a) (LEFT b) 		= a >< b
	(><) (RIGHT a) (RIGHT b) 	= a >< b
	
colorToGen :: Color -> ColorG
colorToGen Red 		= LEFT UNIT
colorToGen Yellow 	= RIGHT (LEFT UNIT)
colorToGen Blue		= RIGHT (RIGHT UNIT)	

instance >< Color where (><) c d = colorToGen c >< colorToGen d

:: TupleG a b :== PAIR a b

tupleToGen :: (a,b) -> TupleG a b
tupleToGen (a,b) 	= PAIR a b	

instance >< (a,b) | >< a & >< b where (><) t u = tupleToGen t >< tupleToGen u

:: TreeG a :== EITHER UNIT (PAIR a (PAIR (Tree a) (Tree a)))

treeToGen :: (Tree a) 	-> TreeG a
treeToGen Tip 			= LEFT UNIT
treeToGen (Bin a x y) 	= RIGHT (PAIR a (PAIR x y))

instance >< (Tree a) | >< a where (><) t u = treeToGen t >< treeToGen u

// 3.2 Yes, they are equal.

// 3.3
// You don't have to write separate functions, just convert to generics.

// 3.4
// Not as efficient, since conversion has to be done first.

Start = (1,2) >< (3,4)
