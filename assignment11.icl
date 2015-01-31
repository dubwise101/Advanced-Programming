module assignment11

import StdEnv

// 1 The Data Type

:: BM a b = { t :: a -> b, f :: b -> a}

:: Term a
	= New			(BM a Set)
	| Insert 		(BM a Set) 	ElemTerm SetTerm
	| Delete 		(BM a Set) 	ElemTerm SetTerm
	| Union 		(BM a Set) 	SetTerm SetTerm
	| Difference 	(BM a Set) 	SetTerm SetTerm
	| Intersection 	(BM a Set) 	SetTerm SetTerm
	| Integer 		(BM a Elem) Int
	| Card 			(BM a Elem) SetTerm 				// cardinality; size
	| Oper 			(BM a Elem) ElemTerm Op ElemTerm
	| ElemVar 		(BM a Elem) Ident 					// read a variable as Element
	| SetVar 		(BM a Set)	Ident 					// read a variable as Set
	| Assign 		Ident 		(Term a)				// any term can be stored
	| E.b: Eq 		(BM a Bool) (Term a) (Term a) 		// all terms of equal type can be compared
	| Less 			(BM a Bool) (Term a) (Term a) 		// all terms of equal type can be compared
	| Seq 			(Term a) 	(Term a) 				// sequence of Terms, the semicolon
	| If 			BoolTerm 	(Term a) (Term a) 		// conditional expression
	| While 		BoolTerm 	(Term a) 				// While loop
:: Op 		= +. | -. | *.
:: Set 		:== [Int]
:: Elem 	:== Int
:: SetTerm 	:== Term Set
:: ElemTerm :== Term Elem
:: Ident 	:== String

:: BoolTerm :== Term Bool

:: State = State [Bind] | Err String
:: Bind = {i :: Ident, v :: Val}
:: Val = I Int | S Ints | B Bool
:: Ints :== [Int]
:: Result a = Succ a State | Fail State
:: Sem a :== State -> Result a

bm :: BM a a
bm = {f = id, t = id}

class store a :: Ident a State -> Result a
instance store Int	 where store i v s = str i v (I v) s
instance store [Int] where store i v s = str i v (S v) s
instance store Bool where store i v s = str i v (B v) s

str :: Ident a Val State -> Result a
str i v w (State s) = Succ v (State (ins i w s))
str i v w error = Fail error

ins :: Ident Val [Bind] -> [Bind]
ins i v [] = [{i=i,v=v}]
ins i v [b=:{i=j,v=w}:r]
	| j > i = [{i=j,v=v},b:r]
	| i > j = [b:ins i v r]
			= [{i=i,v=v}:r]

rtrn :: a -> State -> Result a
rtrn v = \s.case s of Err e = Fail s; = Succ v s

fail :: String -> State -> Result a
fail msg = f where
	f (Err e) = Fail (Err (e +++ "\n" +++ msg))
	f _			= Fail (Err msg)
	
(>>-) infixl 1 :: (Sem a) (a->Sem b) -> Sem b
(>>-) f g = bind where
	bind s=:(Err e) = Fail s
	bind s = case f s of
		Succ v s = g v s 
		Fail s 	 = Fail s
		
union :: [a] [a] -> [a] | < a
union [] ys = ys
union xs [] = xs
union xs=:[a:x] ys=:[b:y]
	| a < b = [a:union x ys]
	| b < a = [b:union xs y]
			= [a:union x y]
/*
(>>.) infix 1 :: Element (Int -> (Sem a)) -> (Sem a)
(>>.) f g = f >>- \x . g x		

(>>..) infix 1 :: Set ([Int] -> (Sem a)) -> (Sem a)
(>>..) f g = f >>- \x . g x*/
	
// 2 Evaluation of Terms
eval :: (Term a) State -> Result a
eval term state = case term of
	New {f} = Succ (f []) state
	Insert {f} e s = (eval e >>- \a.eval s >>- \x. rtrn (f (union [a] x))) state
	Delete {f} e s = (eval e >>- \a.eval s >>- \x. rtrn (f (removeMember a x))) state
	Union {f} s t = (eval s >>- \x.eval t >>- \y. rtrn (f (union x y))) state
	Difference {f} s t = (eval s >>- \x.eval t >>- \y. rtrn (f (filter (\a.not (isMember a y)) x))) state
	Intersection {f} s t = (eval s >>- \x.eval t >>- \y. rtrn (f (filter (\a. (isMember a y)) x))) state
	Integer {f} i = rtrn (f i) state
	Card {f} s = (eval s >>- \x. rtrn (f (length x))) state
	Oper {f} x o y = (eval x >>- \a. eval y >>- \b. rtrn (f (case o of +. = a+b; -. = a-b; *. = a*b))) state
	ElemVar bm ident = Fail state
	SetVar bm ident = Fail state
	Assign ident term = Fail state
	Eq bm t1 t2 = Fail state
	Less bm t1 t2 = Fail state
	Seq t1 t2 = Fail state
	If b t1 t2 = Fail state
	While b t = Fail state
		
Start = 42