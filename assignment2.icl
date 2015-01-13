module assignment2

/*
	Skeleton for Exercise 2 of Advanced Programming.
	Works fine with the environment Everything, but you can also use 
	StdEnv and manually add StdMaybe from the directory {Application}\Libraries\StdLib.
	
	Pieter Koopman, 2013
*/

import StdEnv, StdMaybe

/**************** Prelude *************************/

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)
:: TG		:== CONS UNIT

// Conversions
fromList :: [a]	-> ListG a
fromList []		= LEFT (CONS "Nil" UNIT)
fromList [a:as]	= RIGHT (CONS "Cons" (PAIR a as))

toList :: (ListG a) -> [a]
toList (LEFT (CONS "Nil" UNIT)) 			= []
toList (RIGHT (CONS "Cons" (PAIR a as))) 	= [a:as]

fromTree :: (Tree a) -> (TreeG a)
fromTree Tip 			= LEFT (CONS "Tip" UNIT)
fromTree (Bin t1 a t2) 	= RIGHT (CONS "Bin" (PAIR a (PAIR t1 t2)))

toTree :: (TreeG a) -> (Tree a)
toTree (LEFT (CONS "Tip" UNIT)) 					= Tip
toTree (RIGHT (CONS "Bin" (PAIR a (PAIR t1 t2)))) 	= Bin t1 a t2

fromTuple :: (a,b) -> (TupG a b)
fromTuple (a,b) = CONS "" (PAIR a b)

toTuple :: (TupG a b) -> (a,b)
toTuple (CONS "" (PAIR a b)) = (a,b)

/**************** End Prelude *************************/

// Maarten Derks s4191552

/**************** Part 1 *******************************/

:: Tree a = Tip | Bin (Tree a) a (Tree a)

class Container t
where
	Cinsert   :: a (t a) -> t a      | <        a
	Ccontains :: a (t a) -> Bool     | <, Eq    a
	Cshow     ::   (t a) -> [String] | toString a
	Cnew	  :: t a

instance Container []
where
	Cinsert   :: a ([] a) -> [] a      | <        a
	Cinsert a [] 	= [a]
	Cinsert a [x:xs]
	| a < x			= [a:x:xs]
	| otherwise 	= [x : Cinsert a xs]
	Ccontains :: a ([] a) -> Bool     | <, Eq    a
	Ccontains a [] 	= False
	Ccontains a [x:xs]
	| a == x		= True
	| otherwise		= Ccontains a xs
	Cshow     ::   ([] a) -> [String] | toString a
	Cshow [] 		= []
	Cshow [x:xs] 	= [toString x : Cshow xs]
	Cnew	  :: [] a
	Cnew 			= []

instance Container Tree
where
	Cinsert   :: a (Tree a) -> Tree a      | <        a
	Cinsert a Tip 	= Bin Tip a Tip
	Cinsert a (Bin t1 b t2)
	| a < b			= Bin (Cinsert b t1) a t2
	| otherwise 	= Bin t1 b (Cinsert a t2)
	Ccontains :: a (Tree a) -> Bool     | <, Eq    a
	Ccontains a Tip = False
	Ccontains a (Bin t1 b t2)
	| a == b		= True
	| otherwise		= Ccontains a t1 || Ccontains a t2
	Cshow     ::   (Tree a) -> [String] | toString a
	Cshow Tip 			= ["Tip"]
	Cshow (Bin t1 a t2)	= ["Bin "] ++ Cshow t1 ++ [toString a] ++ Cshow t2
	Cnew	  :: Tree a
	Cnew 			= Tip

// Possible test:
// Start = (Ccontains 3 c,Cshow c) where c = ..

/**************** Part 2 *******************************/
/*
:: IntList = Empty | ConsInt Int IntList
:: List a = Nil | Cons a (List a)
:: Tree a b = Leaf a | Node (Tree a b) b (Tree a b)
:: T1 a b = C11 (a b) | C12 b
:: T2 a b c = C2 (a (T1 b c))
:: T3 a b c = C3 (a b c)
:: T4 a b c = C4 (a (b c))

IntList: *
List: * -> *
List IntList: *
Tree: * -> * -> *
T1: (* -> *) -> * -> *
T2: (* -> *) -> * -> * -> *
T3: (* -> *) -> * -> * -> *
T4: (* -> *) -> (* -> *) -> * -> *
*/

/**************** Part 3 *******************************/
//	Example types
show :: a -> [String] | show_ a
show a = show_ a []

class show_ a
where
	show_ :: a [String] -> [String]

class show0 t :: t [String] -> [String]
class show1 t :: (a [String] -> [String]) (t a) [String] -> [String]        
class show2 t :: (a [String] -> [String]) (b [String] -> [String]) (t a b) [String] -> [String]

instance show0 Int  where show0 i c = ["Int"  : toString i : c]
instance show0 Bool where show0 b c = ["Bool" : toString b : c]

instance show0 UNIT where show0 _ c = ["UNIT" : c]

instance show2 PAIR
where
	show2 f g (PAIR a b) c = ["PAIR" : "(" : f a [")" : "(" : g b [")":c]]]

instance show2 EITHER
where
	show2 f g (LEFT a)  c = ["LEFT" : "(" : f a [")":c]]
	show2 f g (RIGHT b) c = ["RIGHT" : "(" : g b [")":c]]

instance show1 CONS
where
	show1 f (CONS n x) c = ["CONS" : n : "(" : f x [")":c]]
	
instance show1 []
where
	show1 f l c = show2 (show1 show0) (show1 (show2 f (show1 f)))(fromList l) c
	
instance show1 Tree
where
	show1 f t c	= show2 (show1 show0) (show1 (show2 f (show2 (show1 f) (show1 f))))(fromTree t) c

instance show2 (a,b) | show_ a & show_ b
where 
	show2 f g t c = show2 (show1 f) (show1 g) (fromTuple t) c
	
/**************** Part 4 *******************************/
:: Result a = Fail | Match a [String]
class parse a :: [String] -> Result a

class parse0 t :: [String]  -> Result t
class parse1 t :: ([String] -> Result a) [String] -> Result (t a)        
class parse2 t :: ([String] -> Result a) ([String] -> Result b) [String] -> Result (t a b)

getArg :: (Result a) -> a
getArg (Match a _) = a

instance parse Int where
	parse ["Int",i : r]  = Match (toInt i) r
	parse _              = Fail
	
instance parse Bool where
	parse ["Bool",b : r] = Match (b=="True") r
	parse _              = Fail
	
instance parse UNIT where
	parse ["UNIT" : r]   = Match UNIT r
	parse _              = Fail

instance parse2 PAIR 
where
	parse2 f g ["PAIR" : "(" : pair] = getPair f g pair [] [] 0
	where
		getPair :: ([String] -> Result a) ([String] -> Result b) [String] [String] [String] Int -> Result (PAIR a b)
		getPair f g ["(":r] l [] i 		= getPair f g r (l++["("]) [] (i+1)
		getPair f g ["(":r] l l2 i 		= getPair f g r l (l2++["("]) (i+1)
		getPair f g [")":"(":r] l l2 0 	= getPair f g r (l++[")"]) (l2++["("]) 0
		getPair f g [")":r] l l2 0 		= Match (PAIR (getArg(f l)) (getArg(g l2))) r
		getPair f g [")":r] l [] i 		= getPair f g r (l++[")"]) [] (i-1)
		getPair f g [")":r] l l2 i 		= getPair f g r l (l2++[")"]) (i-1)
		getPair f g [a:r] l [] i 		= getPair f g r (l++[a]) [] i
		getPair f g [a:r] l l2 i 		= getPair f g r l (l2++[a]) i
		getPair _ _ [] _ _ _  			= Fail
	
instance parse2 EITHER
where 
	parse2 f g ["LEFT" : "(" : left] = getEitherLeft f left [] 0
	parse2 f g ["RIGHT" : "(" : right] = getEitherRight g right [] 0

getEitherLeft f ["(":r] l i 	= getEitherLeft f r (l++["("]) (i+1)
getEitherLeft f [")":r] l 0		= Match (LEFT (getArg(f l))) r
getEitherLeft f [")":r] l i 	= getEitherLeft f r (l++[")"]) (i-1)
getEitherLeft f [a:r] l i		= getEitherLeft f r (l++[a]) i
getEitherLeft _ _ _ _ 			= Fail

getEitherRight f ["(":r] l i 	= getEitherRight f r (l++["("]) (i+1)
getEitherRight f [")":r] l 0	= Match (RIGHT (getArg(f l))) r
getEitherRight f [")":r] l i 	= getEitherRight f r (l++[")"]) (i-1)
getEitherRight f [a:r] l i		= getEitherRight f r (l++[a]) i
getEitherRight _ _ _ _ 			= Fail
	
instance parse1 CONS 
where
	parse1 f ["CONS" : n : "(" : cons] = getCons f cons [] 0 
	where
		getCons f ["(":r] l i 	= getCons f r (l++["("]) (i+1)
		getCons f [")":r] l 0	= Match (CONS n (getArg(f l))) r
		getCons f [")":r] l i 	= getCons f r (l++[")"]) (i-1)
		getCons f [a:r] l i		= getCons f r (l++[a]) i
		getCons _ _ _ _ 		= Fail

instance parse2 (a,b) | parse a & parse b
where
	parse2 f g t = parse2 (parse1 f) (parse1 g) t

instance parse1 []
where
	parse1 f l = parse2 (parse1 parse0) (parse1 (parse2 f (parse1 f))) l

instance parse1 Tree 
where 
	parse1 f t = parse2 (parse1 parse0) (parse1 (parse2 f (parse2 (parse1 f) (parse1 f)))) t
	

:: T = C

/**************** Starts *******************************/

//Start = ("add your own Start rule!\n", Start4)

// Possible tests:
//Start1 :: ([String],Result T)
//Start1 = (strings,parse strings) where strings = show C

//Start2 :: ([String],Result (Int,Bool))
//Start2 = (strings,parse strings) where strings = show (1,False)

//Start3 :: ([String],Result [Int])
//Start3 = (strings,parse strings) where strings = show l; l :: [Int]; l = [1..4]

/*Start4 :: ([String],Result (Tree Int))
Start4 = (strings,parse strings)
where
	strings = show t
	
	t :: Tree Int
	t = Bin (Bin Tip 2 (Bin Tip 3 Tip)) 4 (Bin (Bin Tip 5 Tip) 6 Tip)*/
