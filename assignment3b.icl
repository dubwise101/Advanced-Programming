module assignment3b

/*
	Advanced Programming.
	Skeleton for exercise 3.3 and 3.4.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdGeneric, StdMaybe, GenEq

// Maarten Derks s4191552

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show_{|Int|}  i c = [toString i:c]
show_{|Bool|} b c = [toString b:c]

show a = show_{|*|} a []

//------------------ parse --------------

:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)
parse{|Bool|} _ = Nothing

//------------------ some data types --------------

:: T		= C
:: Color	= Red | Yellow | Blue
:: Tree a	= Tip | Bin a (Tree a) (Tree a)

//------------------ general useful --------------

instance + String where (+) s t = s+++t
derive bimap Maybe, []

//------------------ to test if parse and show work properly --------------

test :: t -> Bool | gEq{|*|}, show_{|*|}, parse{|*|} t
test x
	= case parse{|*|} (show x) of
		Just (y,[])	= x === y
		_			= False

/**************** End Prelude, add all new code below this line *************************/
show_ {|UNIT|} _ c = c
show_ {|PAIR|} f g (PAIR x y) c = f x (g y c)
show_ {|EITHER|} f g (LEFT x) c = ["LEFT":f x c]
show_ {|EITHER|} f g (RIGHT x) c = ["RIGHT":g x c]
show_ {|CONS of {gcd_name}|} f (CONS x) c = [gcd_name:f x c]
show_ {|OBJECT|} f (OBJECT x) c = f x c

derive show_ T, Color, Tree, [], (,)

parse{|UNIT|} r = Just (UNIT, r) 
parse{|PAIR|} f g r = case f r of 
							Just (a,x) = case g x of
											Just (b,y) 	= Just (PAIR a b,y)
											_ 			= Nothing
							_ = Nothing	
parse{|EITHER|} f g ["LEFT":r] = case f r of Just (l,x) = Just (LEFT l,x); _ = Nothing	
parse{|EITHER|} f g ["RIGHT":r] = case g r of Just (r,x) = Just (RIGHT r,x); _ = Nothing
parse{|CONS of {gcd_name}|} f [n:r] 
| gcd_name == n = case f r of 
					Just (a,b) = Just (CONS a,b)
					_ = Nothing
| otherwise = Nothing
parse{|OBJECT|} f r = case f r of Just (l,x) = Just (OBJECT l,x); _ = Nothing

parse{|Int|} [i:r] = Just (toInt i,r)
parse{|Int|} _ = Nothing

derive parse T, Color, Tree, [], (,)

derive gEq T, Color, Tree

//------------------ tests --------------

Start = [ and [test b \\ b <- [False, True]]
		, and [ test i \\ i <- [0 .. 25]]
		, test [(a,b) \\ a <- [1 .. 2], b <- [5 .. 7]]
		]