module assignment07

import iTasks

// Maarten Derks s4191552

:: Expression
	= New
	| Insert		Element Set
	| Delete		Element Set
	| Variable 		Ident
	| Union 		Set		Set
	| Difference	Set		Set
	| Intersection 	Set		Set
	| Integer		Int
	| Size			Set
	| Assign		Ident 	Expression

:: Ident :== String
:: Val = I Int | S Ints
:: Ints :== [Int]

:: State = State [Bind] | Err String
:: Bind = {i :: Ident, v :: Val}
:: Result a = Succ a State | Fail State
:: Sem a :== State -> Result a
:: Set :== Sem Ints
:: Element :== Sem Int

rtrn :: a -> State -> Result a
rtrn v = \s . case s of	Err	e = Fail s;	= Succ v s	
  
fail :: String -> State -> Result a
fail msg = f where
	f (Err e) 	= Fail (Err (e +++ "\n" +++ msg))
	f _			= Fail (Err msg)

(>>-) infixl 1 :: (Sem a) (a->Sem b) -> Sem b
(>>-) f g = bind where
	bind s=:(Err e) = Fail s
	bind s = case f s of
		Succ v s = g v s 
		Fail s 	 = Fail s
		
readInt :: Ident State -> Result Int
readInt i s=:(State l) = find i l where
	find i [{i=j,v=(I v)}:r]
		| i == j = Succ v s
		| i > j = find i r
	find i [{i=j,v=(S v)}:r] = find i r
	find i l = fail (i +++ " not found") s
readInt i error = Fail error

readSet :: Ident State -> Result [Int]
readSet i s=:(State l) = find i l where
      find i [{i=j,v=(S v)}:r]
              | i == j = Succ v s
              | i > j = find i r
      find i [{i=j,v=(I v)}:r] = find i r
readSet i error = Fail error

class store a :: Ident a State -> Result a
instance store Int	 where store i v (State s) = Succ v (State (ins i (I v) s))
instance store [Int] where store i v (State s) =  Succ v (State (ins i (S v) s))

ins :: Ident Val [Bind] -> [Bind]
ins i v [] = [{i=i,v=v}]
ins i v [b=:{i=j,v=w}:r]
	| j > i = [{i=i,v=v},b:r]
	| i > j = [b:ins i v r]
			= [{i=i,v=v}:r]

union :: [a] [a] -> [a] | < a
union [] ys = ys
union xs [] = xs
union xs=:[a:x] ys=:[b:y]
	| a < b = [a:union x ys]
	| b < a = [b:union xs y]
			= [a:union x y]
			
(>>.) infix 1 :: Element (Int -> (Sem a)) -> (Sem a)
(>>.) f g = f >>- \x . g x		

(>>..) infix 1 :: Set ([Int] -> (Sem a)) -> (Sem a)
(>>..) f g = f >>- \x . g x

instance + Element where
	(+) x y = x >>- \a . y >>- \b . rtrn (a + b)

instance - Element where
	(-) x y = x >>- \a . y >>- \b . rtrn (a - b)

instance * Element where
	(*) x y = x >>- \a . y >>- \b . rtrn (a * b)

evalInt :: Expression -> Element
evalInt expr = case expr of
	Variable i 	= \s . readInt i s
	Integer i 	= rtrn i
	Size s 		= s >>.. \x . rtrn (length x)
	Assign i e 	= evalInt e >>- store i

evalSet :: Expression -> Set	
evalSet expr = case expr of
	New 				= rtrn []
	Insert e s 			= e >>. \a . s >>.. \x . rtrn (union [a] x)
	Delete e s 			= e >>. \a . s >>.. \x . rtrn (removeMember a x)
	Variable v 			= \x . readSet v x
	Union s1 s2 		= s1 >>.. \a . s2 >>.. \x . rtrn (union a x)
	Assign i e 			= evalSet e >>- store i
	Difference s1 s2 	= s1 >>.. \x . s2 >>.. \y . rtrn (filter (\a . not (isMember a y)) x )
	Intersection s1 s2 	= s1 >>.. \x . s2 >>.. \y . rtrn (filter (\a . isMember a y) x )
	Assign i e 			= evalSet e >>- store i

(:.) infixl 1 :: (Sem a) (Sem [a]) -> Sem [a]
(:.) s t = s >>- \x . t >>- \y . rtrn ([x:y])

(==.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | == a
(==.) s t = s >>- \x . t >>- \y . rtrn (x == y)

(<.) infix 4 :: (Sem a) (Sem a) -> Sem Bool | < a
(<.) s t = s >>- \x . t >>- \y . rtrn (x < y)

(=.) infixl 2 :: Ident (Sem a) -> (Sem a) | store a
(=.) x v = v >>- store x

IF :: (Sem Bool) (Sem a) (Sem a) -> Sem a
IF c t f = \s . case c s of        
	(Succ True _) 	= t s
    _          		= f s

//WHILE :: (Sem Bool) (Sem a) -> Sem Int
//WHILE c t = \s . case c s of
//	(Succ True _)	= WHILE c t s
//	_				= t s

derive class iTask Expression, State, Val, Bind, Result

sim :: State Expression -> Task State
sim s e = viewInformation "state" [] s	
	||- updateInformation "expression" [] e
	>>* [OnAction ActionOk (hasValue (\e2 . sim (case evalInt e2 s of Succ v s = s; Fail e = case evalSet e2 s of Succ v s = s; Fail e = e) e2))
		,OnAction ActionNew resetAction
		,OnAction ActionQuit (always (return (State [])))
		] where
	resetAction _ = Just (sim (State []) e)

//expr = 
//	z =. Integer 7 :.
//	x =. New :.
//	x =. insert (variable z) (variable x) :.
//	y =. union (variable x) (variable x) :.
//	WHILE (size (variable x) <. integer 5)
//		(x =. insert (size (variable x)) (variable x)) :.
//	z =. difference (variable x) (intersection (variable x) (insert (variable z) new))

Start :: *World -> *World
Start world
       = startEngine        
       (
          sim (State []) (New)
       )
       world