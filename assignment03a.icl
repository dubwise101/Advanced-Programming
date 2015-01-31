module assignment03a

/*
	Advanced Programming.
	Skeleton for exercise 3.1 and 3.2.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdMaybe

/************* showing *******************/

class show_0 t where show_0 :: t [String] -> [String]
class show_1 t where show_1 :: (a [String] -> [String]) (t a) [String] -> [String]
class show_2 t where show_2 :: (a [String] -> [String]) (b [String] -> [String]) (t a b) [String] -> [String]

instance show_0 Int  where show_0 i    c 			= [toString i:c]
instance show_0 Bool where show_0 b    c 			= [toString b:c]
instance show_0 UNIT where show_0 unit c 			= c
instance show_2 PAIR where show_2 f g (PAIR a b) c 	= f a (g b c)
instance show_2 EITHER where
	show_2 f g (LEFT l) c 							= f l c
	show_2 f g (RIGHT r) c 							= g r c
instance show_1 CONS where show_1 f (CONS n a) c 	= [n:f a c]

IntTag	:== "Int"
BoolTag	:== "Bool"
UNITTag	:== "UNIT"
PAIRTag	:== "PAIR"
CONSTag :== "CONS"

show :: a -> [String] | show_0 a
show a = show_0 a []

/**************** parsing *************************/

:: Result a :== Maybe (a,[String])

class parse0 t :: [String] -> Result t
class parse1 t :: ([String] -> Result a) [String] -> Result (t a)
class parse2 t :: ([String] -> Result a) ([String] -> Result b) [String] -> Result (t a b) 

instance parse0 Int
where
	parse0 [i:r] = Just (toInt i, r)
	parse0 r = Nothing
instance parse0 Bool
where
	parse0 [b:r] = Just (b=="True", r)
	parse0 r = Nothing
instance parse0 UNIT
where
	parse0 r = Just (UNIT, r)
	//parse0 r = Nothing
instance parse2 PAIR
where
	parse2 f g r = case f r of
					Just (a,l) = case g l of
									Just (b,r) = Just (PAIR a b,r)
									_ = Nothing
					Nothing = Nothing
	//parse2 f g r = Nothing
instance parse2 EITHER
where
	parse2 f g c = case f c of
					Just (a,l) = Just (LEFT a,l)
					_ = case g c of
							Just (b,r) = Just (RIGHT b,r)
							_ = Nothing
							
instance parse1 CONS
where
	parse1 f [n:r] = case f r of
								Just (a,r) 	= Just (CONS n a,r)
								_ 			= Nothing
	parse1 f r = Nothing

/**************** Example Types and conversions *************************/

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//	Binary sums and products (in generic prelude)
:: UNIT			= UNIT
:: PAIR   a b	= PAIR a b
:: EITHER a b	= LEFT a | RIGHT b
:: CONS   a		= CONS String a

//	Generic type representations
:: TG		:== CONS UNIT
:: ColorG	:== EITHER (EITHER (CONS UNIT) (CONS UNIT)) (CONS UNIT)
:: ListG a	:== EITHER (CONS UNIT) (CONS (PAIR a [a]))
:: TreeG a	:== EITHER (CONS UNIT) (CONS (PAIR a (PAIR (Tree a) (Tree a))))
:: TupG a b	:== CONS (PAIR a b)

// Conversions

fromT :: T									-> TG
fromT c										= CONS "C" UNIT

fromColor :: Color 							-> ColorG
fromColor Red								= LEFT (LEFT  (CONS "Red"    UNIT))
fromColor Yellow							= LEFT (RIGHT (CONS "Yellow" UNIT))
fromColor Blue								=       RIGHT (CONS "Blue"   UNIT)

fromList :: [a]								-> ListG a
fromList []									= LEFT  (CONS "Nil"  UNIT)
fromList [a:as]								= RIGHT (CONS "Cons" (PAIR a as))

fromTree :: (Tree a)						-> TreeG a
fromTree Tip								= LEFT  (CONS "Tip" UNIT)
fromTree (Bin a l r)						= RIGHT (CONS "Bin" (PAIR a (PAIR l r)))

fromTup :: (a,b)							-> TupG a b
fromTup (a,b)								= CONS "Tuple2" (PAIR a b)

toT :: TG									-> T
toT (CONS _ UNIT)							= C

toColor :: ColorG							-> Color
toColor (LEFT (LEFT  (CONS _ UNIT)))		= Red
toColor (LEFT (RIGHT (CONS _ UNIT)))		= Yellow
toColor       (RIGHT (CONS _ UNIT))			= Blue

toList :: (ListG a)							-> [a]
toList (LEFT  (CONS s UNIT))        		= []
toList (RIGHT (CONS s (PAIR a as)))		 	= [a:as]

toTree :: (TreeG a)							-> Tree a
toTree (LEFT  (CONS s UNIT))                = Tip
toTree (RIGHT (CONS s (PAIR a (PAIR l r)))) = Bin a l r

toTup :: (TupG a b)							-> (a,b)
toTup (CONS s (PAIR a b))					= (a,b)

/**************** to test if parse and show work properly *************************/

test :: t -> Bool | eq0, show_0, parse0 t
test x
	= case parse0 (show x) of
		Just (y,[])	= eq0 x y
		_			= False

/**************** equality with a class for each kind *************************/

class eq0 t ::                              t       t      -> Bool
class eq1 t :: (a a -> Bool)               (t a)   (t a)   -> Bool
class eq2 t :: (a a -> Bool) (b b -> Bool) (t a b) (t a b) -> Bool

instance eq0 UNIT			where eq0 _ _                       = True
instance eq0 Int			where eq0 n m                       = n == m

instance eq1 CONS			where eq1 f   (CONS s x) (CONS t y) = s == t && f x y

instance eq2 PAIR			where eq2 f g (PAIR a b) (PAIR x y) = f a x && g b y
instance eq2 EITHER			where eq2 f g (LEFT  x)  (LEFT  y)  = f x y
							      eq2 f g (RIGHT x)  (RIGHT y)  = g x y
							      eq2 f g _          _          = False

instance eq0 [a] | eq0 a	where eq0   l m = eq1 eq0 l m
instance eq1 []				where eq1 f l m = eq2 (eq1 eq0) (eq1 (eq2 f (eq1 f))) (fromList l) (fromList m)

/**************** map *************************/

class map0 t ::                    t      -> t
class map1 t :: (a -> b)          (t a)   -> t b
class map2 t :: (a -> b) (c -> d) (t a c) -> t b d 

instance map0 Int			where map0 i              = i
instance map0 UNIT			where map0 UNIT           = UNIT

instance map1 CONS			where map1 f   (CONS n x) = CONS n (f x)

instance map2 PAIR			where map2 f g (PAIR x y) = PAIR  (f x) (g y)
instance map2 EITHER		where map2 f g (LEFT  x)  = LEFT  (f x)
							      map2 f g (RIGHT y)  = RIGHT (g y)

/**************** End Prelude *************************/

/**************** please add all new code below this line *************************/

instance eq0 Color		where eq0  c1 c2 		= False		// TO BE IMPROVED
instance ==  Color		where (==) c1 c2 		= eq0 c1 c2	// just to use the well-known notation...
instance show_0 T 		where show_0 t c 		= (show_1 show_0) (fromT t) c
instance show_0 Color 	where show_0 color c 	= show_2 (show_2 (show_1 show_0) (show_1 show_0)) (show_1 show_0) (fromColor color) c
instance show_1 Tree	where show_1 f tree c 	= show_2 (show_1 show_0) (show_1 (show_2 f (show_2 (show_1 f) (show_1 f)))) (fromTree tree) c
instance show_1 []		where show_1 f list c	= show_2 (show_1 show_0) (show_1 (show_2 f (show_1 f))) (fromList list) c
instance show_2 (,)		where show_2 f g tup c	= show_1 (show_2 f g) (fromTup tup) c
instance parse0 T 		where parse0 c 			= case (parse1 parse0) c of
													Just (t,r) = Just (toT t,r)
													_	= Nothing
instance parse0 Color	where parse0 c       	= case (parse2 (parse2 (parse1 parse0) (parse1 parse0)) (parse1 parse0)) c of
													Just (a,r) = Just (toColor a,r)
													_ = Nothing
instance parse1 Tree	where parse1 f c		= case (parse2 (parse1n "Tip" parse0) (parse1n "Bin" (parse2 f (parse2 (parse1 f) (parse1 f))))) c of
													Just (t,r) = Just (toTree t,r)
													_ = Nothing												
instance parse1 []		where parse1 f c		= case (parse2 (parse1n "Nil" parse0) (parse1n "Cons" (parse2 f (parse1 f)))) c of
													Just (l,r) = Just (toList l,r)
													_ = Nothing
instance parse2 (,)		where parse2 f g c		= case (parse1 (parse2 f g)) c of
													Just (t,r) = Just (toTup t,r)
													_ = Nothing

parse1n :: String ([String] -> Result a) [String] -> Result (CONS a)
parse1n s pa [name:x]
| s == name = case pa x of
				Just (a, y) = Just (CONS name a, y)
							= Nothing
parse1n s pa l = Nothing

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

instance map1 []	where map1 f l = (toList o (map2 (map1 map0) (map1 (map2 f (map1 f)))) o fromList) l
instance map1 Tree	where map1 f t = (toTree o (map2 (map1 map0) (map1 (map2 f (map2 (map1 f) (map1 f))))) o fromTree) t
instance map2 (,)	where map2 f g t = (toTup o (map1 (map2 f g)) o fromTup) t

//Start = map1 fac [1 .. 10]
//Start = map1 fac (Bin 2 Tip (Bin 4 Tip Tip))
//Start = map2 (map1 fac) (map1 fac) ([1 .. 10],Bin 2 Tip (Bin 4 Tip Tip))
//Start = map1 (\i.(i,fac i)) [1 .. 10]

// some initial tests, please extend
Start
 =	[ and [ test i \\ i <- [-25 .. 25]]
	, and [ c == toColor (fromColor c) \\ c <- [Red, Yellow, Blue]]
	, and [ test c \\ c <- [Red,Yellow,Blue]]
//	, test [1 .. 3]
//	, test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
//	etc.
	// maps
	, map1 ((+) 1) [0 .. 5] == [1 .. 6]
	]

aTree = Bin 2 Tip (Bin 4 Tip Tip)
