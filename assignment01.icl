module assignment01

/*
	Course I00032 Advanced Programming 2014
	Skeleton for assignment 1
	Pieter Koopman
*/

import StdEnv
import StdOverloaded

/**************** Prelude: *******************************/
// Example types
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)         
:: Rose a	= Rose a [Rose a]

// Binary sums and products (in generic prelude)
:: UNIT		= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b

// Generic type representations
:: RoseG a :== PAIR a [Rose a]

// Conversions
fromRose :: (Rose a) -> RoseG a
fromRose (Rose a l) = PAIR a l

// Oerdering

:: Ordering = Smaller | Equal | Bigger

instance == Ordering where
	(==) Smaller 	Smaller = True
	(==) Bigger 	Bigger 	= True
	(==) Equal 	Equal 	= True
	(==) _ 		_ 	= False

class (><) infix 4 a :: !a !a -> Ordering

instance >< Int where	// Standard ordering for Int
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Char where	// Standard ordering for Char
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< String where// Standard lexicographical ordering
	(><) x y
	| x < y		= Smaller
	| x > y		= Bigger
	| otherwise	= Equal

instance >< Bool where	// False is smaller than True
	(><) False True  = Smaller
	(><) True  False = Bigger
	(><) _     _     = Equal

/**************** 1	Ordering by overloading **************/
/*
instance >< Color where		// Alphabetical ordering
	(><) Blue	Blue	= Equal
	(><) Blue	_	= Smaller
	(><) Red	Blue	= Bigger
	(><) Red	Red	= Equal
	(><) Red	Yellow	= Smaller
	(><) Yellow Yellow	= Equal
	(><) Yellow _		= Bigger

instance >< (Tree a) | >< a	// Ordering from left to right
where
	(><) Tip 				Tip 			= Equal
	(><) Tip				(Bin _ _ _)		= Smaller
	(><) (Bin _ _ _) 		Tip				= Bigger
	(><) (Bin a ax ay)		(Bin b bx by)
	| ((a >< b)==Equal) && ((ax >< bx)==Equal) && ((ay >< by)==Equal) 	= Equal 
	| ((a >< b)==Equal) && ((ax >< bx)==Smaller)				= Smaller 
	| ((a >< b)==Equal) && ((ax >< bx)==Bigger)				= Bigger 
	| ((a >< b)==Equal)							= ay >< by
	| otherwise 								= a >< b

instance >< (Rose a) | >< a // Ordering by subtrees from left to right
where
	(><) (Rose a [])	(Rose b []) 	= a >< b
	(><) (Rose a [])	(Rose b _)	= Smaller
	(><) (Rose a _)		(Rose b [])	= Bigger
	(><) (Rose a [x:xs])	(Rose b [y:ys]) 
	| ((a >< b)==Equal) && ((x >< y)==Equal) 	= Rose a xs >< Rose b ys
	| ((a >< b)==Equal)				= x >< y
	| otherwise					= a >< b

instance >< (a,b) | >< a & >< b	// Lexicographical ordering from left to right
where
	(><) (al,bl) (ar,br)	
	| ((al >< ar)==Equal)	= bl >< br
	| otherwise 		= al >< ar

instance >< [a] | Ord a	// Lexicographical ordering from left to right
where
	(><) []		[]		= Equal	
	(><) []		_		= Smaller
	(><) _		[]		= Bigger	
	(><) [x:xs]	[y:ys]
	| x < y				= Smaller
	| x > y 			= Bigger
	| otherwise			= xs >< ys
*/	
/**************** 2 Generic representation ***************/

// 2.1
:: ColorG :== EITHER Color (EITHER Color Color)
:: ListG a :== EITHER UNIT (PAIR a [a])

// 2.2
listToGen :: [a] -> ListG a
listToGen [] 		= LEFT UNIT
listToGen [a:as]	= RIGHT (PAIR a as)

/* 2.3
   The generic representation of [1,2,3] should be 
   RIGHT (PAIR 1 [2,3]). This is indeed the value obtained
   by listToGen [1,2,3]. */
	
/* 2.4
   No, this is not possible. Type correctness of programs 
   would become undecidable. */
	
/*************** 3 Ordering via a generic representation */

instance >< [a] | >< a
where 
	(><) l m = listToGen l >< listToGen m

// 3.1
// Define instance of >< for the types UNIT, PAIR, and EITHER:
instance >< UNIT where
	(><) UNIT UNIT 	= Equal
	
instance >< (PAIR a b) | >< a & >< b where
	(><) (PAIR x1 x2) (PAIR y1 y2)
	| ((x1 >< y1)==Equal)	= x2 >< y2
	| otherwise		= x1 >< y1
	
instance >< (EITHER a b) | >< a & >< b
where
	(><) (LEFT a) 		(LEFT b)	= Equal
	(><) (LEFT a)		(RIGHT b)	= Smaller
	(><) (RIGHT a)		(LEFT b)	= Bigger
	(><) (RIGHT a)		(RIGHT b)	= a >< b

// Use these to implement the ordering on Color, (a,b), and Tree a:

// Color	
colorToGen :: Color -> ColorG
colorToGen Blue 	= LEFT Blue
colorToGen Red		= RIGHT (LEFT Red)
colorToGen Yellow 	= RIGHT (RIGHT Yellow)
	
instance >< Color where
	(><) a b = colorToGen a >< colorToGen b

// (a,b)
:: TupleG a b	:== PAIR a b

tupleToGen :: (a,b) -> TupleG a b
tupleToGen (a,b) = PAIR a b

instance >< (a,b) | >< a & >< b
where
	(><) t1 t2 = tupleToGen t1 >< tupleToGen t2

// Tree
:: TreeG a :== EITHER UNIT (PAIR a (PAIR (Tree a) (Tree a)))

treeToGen :: (Tree a) -> TreeG a
treeToGen Tip		= LEFT UNIT
treeToGen (Bin a x y)	= RIGHT (PAIR a (PAIR x y)) 

instance >< (Tree a) | >< a
where
	(><) a b = treeToGen a >< treeToGen b

// 3.2 
// Yes, the results are the same.
	
/* 3.3 
   It's easier and less work. You have to define less/
   nothing by hand, just translate it to generic, that 
   will do the rest for you. */
	
/* 3.4 
   Not as efficient as the hand made one, since 
   conversion has to be done first. */
	
/**************** End Prelude *************************/

Start = Blue >< Blue		// Equal
//Start = Blue >< Red		// Smaller
//Start = Blue >< Yellow	// Smaller
//Start = Red >< Blue		// Bigger
//Start = Red >< Red		// Equal
//Start = Red >< Yellow		// Smaller
//Start = Yellow >< Blue	// Bigger
//Start = Yellow >< Red		// Bigger
//Start = Yellow >< Yellow	// Equal

//Start = Tip >< Tip		// Equal
//Start = Tip >< Bin 0 Tip Tip	// Smaller
//Start = Bin 0 Tip Tip >< Tip 	// Bigger
//Start = Bin 0 Tip Tip >< Bin 0 Tip Tip	// Equal
//Start = Bin 0 Tip Tip >< Bin 1 Tip Tip	// Smaller
//Start = Bin 1 Tip Tip >< Bin 0 Tip Tip	// Bigger
//Start = Bin 3 (Bin 4 Tip Tip) (Bin 4 Tip Tip) >< Bin 3 Tip Tip	// Bigger
//Start = Bin 0 (Bin 1 Tip Tip) Tip >< Bin 0 (Bin 1 Tip Tip) Tip 	// Equal
//Start = Bin 0 (Bin 1 Tip Tip) Tip >< Bin 0 (Bin 2 Tip Tip) Tip		// Smaller
//Start = Bin 0 (Bin 2 Tip Tip) Tip >< Bin 0 (Bin 1 Tip Tip) Tip		// Bigger
//Start = Bin 0 (Bin 0 Tip Tip) Tip >< Bin 0 (Bin 0 Tip Tip) (Bin 0 Tip Tip) // Smaller
//Start = Bin 0 (Bin 1 (Bin 2 (Bin 3 Tip Tip) Tip) (Bin 4 Tip Tip)) Tip >< Bin 0 (Bin 1 (Bin 2 (Bin 4 Tip Tip) Tip) Tip) Tip	// Smaller

//Start = Rose 0 [] >< Rose 0 []			// Equal
//Start = Rose 0 [] >< Rose 1 []			// Smaller
//Start = Rose 1 [] >< Rose 0 []			// Bigger
//Start = Rose 0 [(Rose 0 [])] >< Rose 0 [(Rose 1 [])]	// Smaller
//Start = Rose 0 [(Rose 1 [])] >< Rose 0 [(Rose 0 [])]	// Bigger

//Start = (0,0) >< (0,0)	// Equal
//Start = (0,0) >< (0,1)	// Smaller
//Start = (1,0) >< (0,0)	// Bigger

//Start = [] >< []		// Gaat fout?
//Start = [] >< [0]		// Smaller
//Start = [0] >< []		// Bigger
//Start = [0,1] >< [0]
//Start = [1,2,3,4,6] >< [1,2,3,4,5]	// Bigger
//Start = [[1..3] >< [1..2], [1..2] >< [1..5]]	// [Bigger,Smaller]
//Start = [1..2] >< [1..3] 
//Start = listToGen [1,2,3] >< listToGen [1,2,4]
//Start = listToGen [1,2,3]
//Start = tupleToGen (0,1) >< tupleToGen (0,2)
