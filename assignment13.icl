module assignment13

import StdEnv
//import iTasks

// Maarten Derks

class arith x where
	lit :: a -> x a | toString a
	(+.) infixl 6 :: (x a) (x a) -> x a | + a // integer addition, Boolean OR
	(*.) infixl 7 :: (x a) (x a) -> x a | * a // integer multiplication, Boolean AND

class store x where
	read :: (x Int)
	write :: (x Int) -> x Int

class truth x where
	(XOR) infixr 3 :: (x Bool) (x Bool) -> x Bool
	-. :: (x Bool) -> x Bool

class (=.=) infix 4 x :: (x a) (x a) -> x Bool | == a

class except x where
	throw :: (x a)
	try :: (x a) (x a) -> x a

class aexpr x | arith, store, except, =.= x
class bexpr x | arith, truth, except, =.= x
class expr x  | aexpr, bexpr x

:: Show a = Show ([String] -> [String])

instance arith Show where  
	lit i    				= Show \s.[toString i:s]  
	(+.) (Show x) (Show y) 	= Show \s.["(":x["+":y[")":s]]]
	(*.) (Show x) (Show y) 	= Show \s.["(":x["*":y[")":s]]]
	
instance store Show where
	read 					= Show \s.["read":s]
	write (Show x) 			= Show \s.["write ": x s]
	
instance truth Show where
	(XOR) (Show x) (Show y) = Show \s.["(":x["XOR":y[")":s]]]
	(-.) (Show x) 			= Show \s.["(-.":x[")":s]]

instance =.= Show where
	(=.=) (Show x) (Show y) = Show \s.["(":x["==":y[")":s]]]

instance except Show where
	throw 					= Show \s.["throw": s]
	try (Show x) (Show y) 	= Show \s.["(try ": x [" ": y [")":s]]]
		
// 2 Evaluation
:: Maybe a = Just a | Nothing

:: Step a = Step (State -> (Maybe a, State))
:: State :== Int

class monad m where
	rtrn :: a -> (m a)
	(>>=) infixl 1 :: (m a) (a->(m b)) -> (m b)
	fail :: m a
	
instance monad Step where
	rtrn a 					= Step \s.(Just a,s)
	fail 					= Step \s.(Nothing,s)
	(>>=) f g 				= Step \s.let (ma,t) = f s in
								case ma of
									Just a = g a (ma,t)
									Nothing = (Nothing,t)
						
instance arith Step where  
	lit i    				= rtrn i
	(+.) (Step x) (Step y) 	= x >>= \a. y >>= \b . rtrn (a + b)
	(*.) (Step x) (Step y) 	= x >>= \a. y >>= \b . rtrn (a * b)

instance store Step where
	read 					= Step \s.(Just s, s)
	write (Step x) 			= Step \s.let (ma,t) = x s in
								case ma of
									Just a 	= (Just a, a)
									_		= (ma,t)

instance truth Step where
	(XOR) (Step x) (Step y) = Step \s.(Nothing,s)
	(-.) (Step x) 			= x >>= \a. rtrn (not a)
	
instance =.= Step where
	(=.=) (Step x) (Step y) = x >>= \a. y >>= \b . rtrn (a == b)

instance except Step where
	throw 		= fail
	try f g 	= Step \s.let (ma,t) = f s in
								case ma of
									Just a = (ma, t)
									_ = g (ma,t)

// 3 Examples
seven :: e Int | aexpr e
seven = lit 3 +. lit 4

throw1 :: e Int | expr e
throw1 = lit 3 +. throw

six :: e Int | expr e
six = write (lit 3) +. read

try1 :: e Int | expr e
try1 = try throw1 (lit 42)

loge :: e Bool | expr e
loge = lit True *. -. (lit True)

comp :: e Bool | expr e
comp = lit 1 =.= lit 2 XOR -. (-. (lit True))
				
Start = comp