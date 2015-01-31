module assignment06

import iTasks

:: Expression
	= New
	| Insert Element Set
	| Delete Element Set
	| Variable Ident
	| Union Set Set
	| Difference Set Set
	| Intersection Set Set
	| Integer Int
	| Size Set
	| Oper Element Op Element
	| Assign Ident Expression
:: Op = +. | -. | *.
:: Set :== Expression
:: Element :== Expression
:: Ident :== String

// 1 State
:: State = Err String | State [Bind]
:: Bind :== (Var,Val)
:: Var :== String
:: Val = I Int | S Ints
:: Ints :== [Int]

// 2 State Manipulations
:: Result = Fail State | Succ Val State
:: Sem :== State -> Result

rtrn :: Val -> Sem
rtrn v = \s.case s of Err e = Fail s; = Succ v s

store :: Var Val State 	-> Result
store i v (State s) 	= Succ v (State (ins i v s))
store i	v error			= Fail error

ins :: Var Val [(Var,Val)] -> [(Var,Val)]	
ins v x [] = [(v,x)]
ins v x [(var,val):xs]
| v == var = [(var,x):xs]
			= ins v x xs

read :: Var State -> Result
read i (State s) = find i s
read i error = Fail error

find :: Var [(Var,Val)] -> Result
find v []	= Fail (Err "var not found"	)
find v [(a,b):xs]
| v == a 	= Succ b (State [(a,b):xs])
			= find v xs

fail :: String -> Sem 
fail msg = f where
	f (Error e) = Fail (Err (e +++ "\n" +++ msg))
	f _			= Fail (Err msg)

(>>-) infixl 1 :: Sem (Val->Sem) -> Sem
(>>-) f g = bind where
	bind s=:(Error e) = Fail s
	bind s = case f s of 
		Succ v s = g v s
		Fail s = Fail s
		
(>>.) infixl 1 :: Sem (Int->Sem) -> Sem
(>>.) f g = f >>- \x.case x of
	I i = g i
	S s = fail "Element expected instead of set."
	
(>>..) infixl 1 :: Sem ([Int]->Sem) -> Sem
(>>..) f g = f >>- \x.case x of
	S s = g s
	I i = fail "Set expected instead of element."

union :: [a] [a] -> [a] | < a
union [] ys = ys
union xs [] = xs
union xs=:[a:x] ys=:[b:y]
| a < b = [a:union x ys]
| b < a = [b:union xs y]
		= [a:union x y]

// 3 Evaluator
eval :: Expression -> Sem
eval e = case e of
	Insert e s 			= eval e >>. \a.eval s >>.. \x.rtrn (S(union [a] x))
	Delete e s 			= eval e >>. \a.eval s >>.. \x.rtrn (S(removeM a x))
	Variable ident 		= read ident
	Union s t 			= eval s >>.. \x.eval t >>.. \y.rtrn (S (union x y))
	Difference s t 		= eval s >>.. \x.eval t >>.. \y. rtrn (S (filter (\a.not (isMember a y)) x))
	Intersection s t 	= eval s >>.. \x.eval t >>.. \y. rtrn (S (filter (\a.isMember a y) x))
	Integer int 		= rtrn (I int)
	Size s				= eval s >>.. \x. rtrn (I (length x))
	Oper x o y 			= eval x >>. \a.eval y >>. \b.rtrn (I (case o of +. = a+b; -. = a-b; *. = a*b))
	Assign v e 			= eval e >>- store v

removeM = removeMember

derive class iTask Expression, Op, State, Val, Bind

sim :: State Expression -> Task Int
sim s e = 
	(	viewInformation "state" [] s
	||- viewInformation "last expression" [] (pp{|*|} e)
	||- updateInformation "expression" [] e
	)
	>>* [OnAction ActionOk
	(hasValue (\e2.sim (case eval e2 s of Succ v s = s; Fail e = e) e2))
	,OnAction ActionNew resetAction
	,OnAction ActionQuit (always (return 7))
	] where
	resetAction (Value e _) = Just (sim (State []) e)
	resetAction _ = Just (sim (State []) e)


Start = 42