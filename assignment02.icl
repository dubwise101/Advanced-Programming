module assignment02

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
toList (LEFT (CONS "Nil" UNIT)) = []
toList (RIGHT (CONS "Cons" (PAIR a as))) = [a:as]


/**************** End Prelude *************************/

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
	Cinsert 	a c 	= [a:c]
	Ccontains 	a c		= isMember a c
	Cshow 		  c 	= ["{":showElems c ["}"]]
	Cnew 				= []

showElems [] 	 c = c
showElems [x]	 c = [toString x:c]
showElems [x:xs] c = [toString x,",":showElems xs c]

instance Container Tree
where
	Cinsert a Tip 			= Bin Tip a Tip
	Cinsert a (Bin l b r)
	| a < b					= Bin (Cinsert a l) b r
	| otherwise 			= Bin l b (Cinsert a r)
	Ccontains a Tip 		= False
	Ccontains a (Bin l b r)
	| a < b 				= Ccontains a l
	| a > b 				= Ccontains a r
	| otherwise				= True
	Cshow t 				= ["{":showElems (TreetoList t []) ["}"]]
	Cnew 					= Tip

TreetoList Tip c = c
TreetoList (Bin l a r) c = TreetoList l [a: TreetoList r c]
	
// Possible test:
//Start = (Ccontains 3 c,Cshow c) where c = ...

/**************** Part 2 *******************************/

/*
IntList: *
List: * -> *
List IntList: *
Tree: * -> * -> *
T1: (* -> *) -> * -> *
T2: (* -> *) -> (* -> *) -> * -> *
T3: (* -> * -> *) -> * -> * -> *
T4: (* -> *) -> (* -> *) -> * -> *
*/

/**************** Part 3 *******************************/

//	Example types
show :: a -> [String] | show_ a
show a = show_ a []

class show_ a where show_ :: a [String] -> [String]

instance show_ Int  where show_ i c = ["Int"  : toString i : c]
instance show_ Bool where show_ b c = ["Bool" : toString b : c]

instance show_ UNIT where show_ _ c = ["UNIT" : c]
instance show_ (PAIR a b) | show_ a & show_ b where show_ (PAIR a b) c = ["PAIR":show_ a (show_ b c)]
instance show_ (EITHER a b) | show_ a & show_ b where
	show_ (LEFT a) c = ["LEFT":show_ a c]
	show_ (RIGHT b) c = ["RIGHT":show_ b c]
instance show_ (CONS a) | show_ a where show_ (CONS name a) c = ["CONS",name:show_ a c]

instance show_ [a] | show_ a where show_ l c = show_ (fromList l) c

fromTree :: (Tree a) 	-> TreeG a
fromTree Tip 			= LEFT (CONS "Tip" UNIT)
fromTree (Bin l a r)	= RIGHT (CONS "Bin" (PAIR a (PAIR l r)))

instance show_ (Tree a) | show_ a where show_ t c = show_ (fromTree t) c

fromTup :: (a,b)	-> TupG a b
fromTup (a,b)		= CONS "Tup" (PAIR a b)

instance show_ (a,b) | show_ a & show_ b where show_ t c = show_ (fromTup t) c

/**************** Part 4 *******************************/

:: Result a = Fail | Match a [String]
class parse a :: [String] -> Result a

instance parse Int where
	parse ["Int",i : r]  	= Match (toInt i) r
	parse _              	= Fail
instance parse Bool where
	parse ["Bool",b : r] 	= Match (b=="True") r
	parse _              	= Fail
instance parse UNIT where
	parse ["UNIT" : r]  	= Match UNIT r
	parse _             	= Fail
instance parse (PAIR a b) | parse a & parse b where
	parse ["PAIR" : p]		= case parse p of
								Match a l 	= case parse l of
												Match b r = Match (PAIR a b) r
												_ = Fail
								_ 			= Fail				
	parse l = Fail
instance parse (EITHER a b) | parse a & parse b  where
	parse ["LEFT" : l] = case parse l of
							Match a r 	= Match (LEFT a) r
							_ 			= Fail
	parse ["RIGHT" : l] = case parse l of
							Match b r 	= Match (RIGHT b) r
							_ 			= Fail
	parse l = Fail
instance parse (CONS a) | parse a where
	parse ["CONS",name:l] = case parse l of
							Match a r 	= Match (CONS name a) r
							_			= Fail 
	parse l = Fail

toTup :: (TupG a b) -> (a,b)
toTup (CONS "Tup" (PAIR a b))		= (a,b)

instance parse (a,b) | parse a & parse b where parse tuple = case parse tuple of
													Match t r 	= Match (toTup t) r
													_ 			= Fail
instance parse [a] | parse a where parse list = case parse list of
													Match l r 	= Match (toList l) r
													_ 			= Fail

toTree :: (TreeG a) -> Tree a
toTree (LEFT (CONS "Tip" UNIT)) 				= Tip
toTree (RIGHT (CONS "Bin" (PAIR a (PAIR l r)))) = Bin l a r

instance parse (Tree a) | parse a where parse tree = case parse tree of
													Match t r 	= Match (toTree t) r
													_ 			= Fail
:: T = C

/**************** Starts *******************************/

Start = ("add your own Start rule!\n", Start4)

// Possible tests:
//Start1 :: ([String],Result T)
//Start1 = (strings,parse strings) where strings = show C

//Start2 :: ([String],Result (Int,Bool))
//Start2 = (strings,parse strings) where strings = show (1,False)

//Start3 :: ([String],Result [Int])
//Start3 = (strings,parse strings) where strings = show l; l :: [Int]; l = [1..4]

Start4 :: ([String],Result (Tree Int))
Start4 = (strings,parse strings)
where
	strings = show t
	
	t :: Tree Int
	t = Bin (Bin Tip 2 (Bin Tip 3 Tip)) 4 (Bin (Bin Tip 5 Tip) 6 Tip)
