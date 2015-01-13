module assignment12

import StdEnv
import StdFile

// Maarten Derks

class monad m where
	rtrn :: a -> (m a)
	(>>=) infixl 1 :: (m a) (a->(m b)) -> (m b)
	
class failMonad m | monad m where
	fail :: String -> m a

(>>|) infixl 1 :: (m a) (m b) -> (m b) | monad m
(>>|) f g = f >>= \x. g

// 1 The State
:: Error :== String
:: Val :== Int
:: Var :== String
:: Bind = {i :: Var, v :: Val}

:: *State = { bindings :: [Bind], io :: *File, errors :: [Error] } 

// 2 The Monad for this State
:: M a = M (*State->*(a, *State))

instance monad M where
	rtrn a = M \f.(a,f)
	(>>=) (M f) g = M \s = let (a,t) = f s; (M h) = g a in h t

instance failMonad M where
	fail a = abort a	
	
// 3 Conditions
cond :: Bool String -> m a | failMonad m 
cond False b = fail b
cond True b = rtrn undef

// 4 Arithmetic Expressions
instance + (m a) | + a & monad m where
	(+) x y = x >>= \a . y >>= \b -> rtrn (a+b)
	
instance - (m a) | - a & monad m where
	(-) x y = x >>= \a . y >>= \b -> rtrn (a-b)
	
instance * (m a) | * a & monad m where
	(*) x y = x >>= \a . y >>= \b -> rtrn (a*b)
	
instance / (m x) | /, ==, zero x & failMonad m  where
	(/) x y = x >>= \a . y >>= \b -> cond (b <> zero) "Cannot divide by zero!" >>| rtrn (a / b)

// 5 Variables
var :: String -> M Int
var a = M \s=:{bindings = b} . (getval b,s)
where
	getval :: [Bind] -> Int
	getval [] = 0;
	getval [{i=id,v=val}:r]
	| id == a = val
	| otherwise = getval r
	
(:=.) infix 2 :: Var (M Int) -> (M Int)
(:=.) v (M f) = M (\s . advar (f s) v)
where
	advar :: (Int,State) Var -> (Int,State)
	advar (a,s=:{bindings = b, io, errors}) v = (a,{bindings = setvar a b v, io, errors})
	where 
		setvar :: Int [Bind] Var -> [Bind]
		setvar i [] v = [{i=v,v=i}]
		setvar i [{i=id,v=val}:r] v
			| id == v = [{i=id,v=i}:r]
			| otherwise = [{i=id,v=val}:setvar i r v]
	
// 6 Input and Output
//read :: M a | parse a

//print :: a -> M a | <<< a

//class parse a :: String -> Maybe a

// 7 Control Structures
//IF :: (m Bool) (m a) (m a) -> m a | monad m	

//(=.=) infix 4 :: (m a) (m a) -> m Bool | == a & monad m

//try :: (M a) (M a) -> M a
	
// 8 Example
//prog =
//	print "Enter x " >>|
//	"x" :=. read >>|
//	print "Enter y " >>|
//	"y" :=. read >>|
//	try
//	( 	print "x / y = " >>|
//		var "x" / var "y" >>= print
//	)
//	(	print "Cannot compute x / y" >>|
//		Int 0
//	)

Start = 42