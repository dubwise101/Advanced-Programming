module assignment03b

/*
	Advanced Programming.
	Skeleton for exercise 3.3 and 3.4.
	To be used in a project with the environment Everything, 
	or StdEnv with an import of StdMaybe from StdLib

	Pieter Koopman, pieter@cs.ru.nl
*/

import StdEnv, StdGeneric, StdMaybe, GenEq

//------------------ show --------------
generic show_ a :: a [String] -> [String]

show_{|Int|}  i c 							= [toString i:c]
show_{|Bool|} b c 							= [toString b:c]
show_{|UNIT|} UNIT c 						= c
show_{|PAIR|} f g (PAIR a b) c 				= f a (g b c)
show_{|EITHER|} f g (LEFT l) c				= f l c
show_{|EITHER|} f g (RIGHT r) c				= g r c
show_{|CONS of {gcd_name, gcd_arity}|} sa (CONS a) c
| gcd_arity >0 	= ["(",gcd_name:sa a [")":c]]
| otherwise 	= [gcd_name:sa a c]
show_{|OBJECT|} f (OBJECT a) c				= f a c

derive show_ (,),[],Tree,T

show a = show_{|*|} a []

//------------------ parse --------------

:: Result a :== Maybe (a, [String])

generic parse a :: [String] -> Result a

parse{|Int|} [string:r]
| size string>0 && (isDigit string.[0] || string.[0] =='-') = Just (toInt string, r)
parse{|Int|} _ = Nothing

parse{|Bool|} ["True" :r] = Just (True ,r)
parse{|Bool|} ["False":r] = Just (False,r)
parse{|Bool|} _ = Nothing

parse{|UNIT|} r = Just (UNIT,r)
parse{|PAIR|} f g p = case f p of
								Just (a,p) = case g p of
												Just (b,r) = Just (PAIR a b,r)
												_ = Nothing
								Nothing = Nothing
parse{|EITHER|} f g e = case f e of
							Nothing = case g e of
										Just (b,r) = Just (RIGHT b,r)
										Nothing = Nothing
							Just (a,r) = Just (LEFT a,r) 
parse{|CONS of {gcd_name, gcd_arity}|} pa [s:r]
| gcd_arity > 0 && s == "(" && not (isEmpty r) && hd r == gcd_name = case pa (tl r) of
																		Just (a, [")": r]) 	= Just (CONS a, r)
																							= Nothing
| s == gcd_name = case pa r of
				Just (a, r) = Just (CONS a, r)
				= Nothing
= Nothing
							
parse{|OBJECT|} f r = case f r of
						Just (a,r) = Just (OBJECT a,r)
						_ = Nothing 
						
derive parse (,),[],Tree,T
derive gEq Tree

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

//------------------ tests --------------

//Start = and [test b \\ b <- [False, True]]
Start = test ([1 .. 10],Bin 2 Tip (Bin 4 Tip Tip))
