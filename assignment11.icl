module assignment11

import StdEnv

// Maarten Derks

// 1 The Data Type
:: Term a
	= New			(BM a [a])
	| Insert 		(BM a [a]) (ElemTerm a) (SetTerm a)
	| Delete 		(BM a [a]) (ElemTerm a) (SetTerm a)
	| Union 		(BM a [a]) (SetTerm a) (SetTerm a)
	| Difference 	(BM a [a]) (SetTerm a) (SetTerm a)
	| Intersection 	(BM a [a]) (SetTerm a) (SetTerm a)
	| Integer 		(BM a Int) Int
	| Card 			(BM a Int) (SetTerm a) 			// cardinality; size
	| Oper 			(BM a Int) (ElemTerm Int) Op (ElemTerm Int)
	| ElemVar 		(BM a Val) Ident 				// read a variable as Element
	| SetVar 		(BM a Val) Ident 				// read a variable as Set
	| Assign 		Ident (Term a)					// any term can be stored
	| Eq 			(BM a Bool) (Term a) (Term a) 	// all terms of equal type can be compared
	| Less 			(BM a Bool) (Term a) (Term a) 	// all terms of equal type can be compared
	| Seq 			(Term a) (Term a) 				// sequence of Terms, the semicolon
	| If 			BoolTerm (Term a) (Term a) 		// conditional expression
	| While 		(BM a [a]) BoolTerm (Term a)	// While loop
:: Op = +. | -. | *.
:: Ident :== String

:: ElemTerm a :== a
:: SetTerm a :== [a]
:: BoolTerm :== Bool

:: BM a b = { t :: a -> b, f :: b -> a }

// 2 Evaluation of Terms
:: Val = I Int | S Ints
:: Ints :== [Int]

:: State = State [Bind] | Err String
:: Bind = {i :: Ident, v :: Val}
:: Result a = Succ a State | Fail State
:: Sem a :== State -> Result a
  
eval :: (Term a) State -> Result a | Eq, Ord a
eval (New {f}) s 				= Succ (f []) s
eval (Insert {f} e s) st 		= Succ (f (s++[e])) st
eval (Delete {f} x y) s 		= Succ (f (removeMember x y)) s
eval (Union {f} x y) s 			= Succ (f (x ++ y)) s
eval (Difference {f} x y) s 	= Succ (f (filter (\a.not (isMember a y)) x)) s
eval (Intersection {f} x y) s 	= Succ (f (filter (\a.isMember a y) x)) s 
eval (Integer {f} i) st 		= Succ (f i) st
eval (Card {f} s) st 			= Succ (f (length s)) st
eval (Oper {f} x (+.) y) s 		= Succ (f (x + y)) s
eval (Oper {f} x (-.) y) s 		= Succ (f (x - y)) s
eval (Oper {f} x (*.) y) s 		= Succ (f (x * y)) s
eval (ElemVar {f} i) s      	= Succ (f (readvar False s i)) s
eval (SetVar {f} i) s      		= Succ (f (readvar True s i)) s
//eval (Assign i t) s 			= store i t s
eval (Eq {f} x y) s 			= Succ (f (eval x s == eval y s)) s
eval (Less {f} x y) s 			= Succ (f (eval x s < eval y s)) s
eval (Seq x y) s          		= case eval x s of Succ _ s = eval y s; _ = Fail s
eval (If b t0 t1) s 			= case b of True = eval t0 s; False = eval t1 s
eval (While a b t) st      		= case b of 
									True  = dowhile t b a (eval t st)
                                    False = eval (New a) st
               where
               dowhile :: (Term a) BoolTerm (BM a [a]) (Result a) -> (Result a) | Eq, Ord a
               dowhile t1 b a (Succ _ st) = eval (While a b t1) st
               dowhile _ _ _ (Fail state)= Fail state

//class store a :: Ident a State -> Result a
//instance store (ElemTerm a) where store i v s = Fail (Err "")//str i (I v) s
//instance store SetTerm where store i v s = str i v s

//str :: Ident a State -> Result a
//str i v (State s) = Succ v (State (ins i v s))
//str i v error = Fail error

ins :: Ident Val [Bind] -> [Bind]
ins i v [] = [{i=i,v=v}]
ins i v [b=:{i=j,v=w}:r] 
| j > i = [{i=i,v=v},b:r] 
| i > j = [b:ins i v r]
		= [{i=i,v=v}:r] 

instance == (Result a) | Eq a where
	(==) (Succ a x) (Succ b y) = a == b
	
instance < (Result a) | Ord a where
	(<) (Succ a x) (Succ b y) = a < b
	
readvar :: Bool State Ident -> Val
readvar _ (Err s) i = S []
readvar b (State a) i = findid b a i
       where
       findid :: Bool [Bind] Ident -> Val
       findid _ [] _     = S []
       findid True [{i=s, v=(S a)}:r] iden
               | s == iden = (S a)
               | iden > s  = S []
               | otherwise = findid True r iden
       findid False [{i=s, v=(I a)}:r] iden
               | s == iden = (I a)
               | iden > s  = S []
               | otherwise = findid False r iden
       findid b [{i=s, v=var}:r] iden = findid b r iden
               
instance < (Ident)
where
      (<) i1 i2 = i1 < i2

instance == (Ident)
where
      (==) i1 i2 = i1 == i2        
           
// 3 Showing Terms
class show a :: a [String] -> [String]

instance show (Term a) | toString a where
	show (New _) 				s = ["[]":s]
	show (Insert _ x y) 		s = ["Insert" : toString  x : [" ": show y [" ":s]]]
	show (Delete _ x y) 		s = ["Delete" : toString x : [" " : show y [" ":s]]]
	show (Union _ x y) 			s = ["(":show x ["\/": show y [")": s]]]
	show (Difference _ x y) 	s = ["Difference ": show x []] ++ [" ":show y s]
	show (Intersection _ x y) 	s = ["(":show x ["/\\": show y [")": s]]]
	show (Integer _ i) 			s = [toString i: s] 
	show (Card _ x) 			s = ["Card ": show x s]
	show (Oper _ x (+.) y) 		s = ["(": toString x :"+": toString y : ")":s]
	show (Oper _ x (-.) y) 		s = ["(": toString x :"-": toString y :")":s]
	show (Oper _ x (*.) y) 		s = ["(": toString x :"*": toString y :")":s]
	show (ElemVar _ i) 			s = [i:s]
	show (SetVar _ i) 			s = [i:s]
	show (Assign x y) 			s = ["(":x: "=": show y [ ")": s]]
	show (Eq _ x y) 			s = ["(":show x ["==": show y [")": s]]]
	show (Less _ x y) 			s = ["(":show x ["<": show y [")": s]]]
	show (Seq x y) 				s = ["(":show x [";": show y [")": s]]]
	show (If b t0 t1) 			s = ["if (":toString b:") then ": show t0 [" else ": show t1 s]]
	show (While a b t) 			s = ["while (":toString b:") do ": show t s]

instance show (SetTerm a) | toString a where
	show [] s = ["[]":s]
	show [x:y] s = [toString x:show y [":": s]]
	
// 4 Syntactic Sugar
bm :: BM a a
bm = {f = id, t = id} 

bmI :: BM a Int | toInt, fromInt  a
bmI = {f  = fromInt, t = toInt}

bmB :: BM a Bool | toBool, fromBool a
bmB = {f  = fromBool, t = toBool}

bmL :: BM a [a]
bmL = {f = fromlist, t = toList}
where
	toList :: a -> [a]
	toList a = [a]

	fromlist :: [a] -> a
	fromlist [a:b] = a

new = New bmL
elem v = ElemVar v
int i = Integer bmI i
add e s = Insert e s
setv i = SetVar i
card a = Card a

instance + (Term Int) where 
	(+) (Integer f x) (Integer g y) = Oper f x (+.) y
	
instance - (Term Int) where
	(-) (Integer f x) (Integer g y) = Oper f x (-.) y	

instance * (Term Int) where
	(*) (Integer f x) (Integer g y) = Oper f x (*.) y

(<.) infixr 2 :: (Term a) (Term a) -> (Term a) | toBool, fromBool a
(<.) x y = Less bmB x y

(==.) infixr 2 :: (Term a) (Term a) -> (Term a) | toBool, fromBool a
(==.) x y = Eq bmB x y

(=.) infixr 2 :: Ident (Term a) -> Term a
(=.) a b = Assign a b

(:.) infixr 2 :: (Term a) (Term a) -> Term a
(:.) a b = Seq a b

(U) infixr 2 :: (SetTerm a) (SetTerm a) -> (Term a)
(U) x y  = Union bmL x y

(n) infixr 2 :: (SetTerm a) (SetTerm a) -> (Term a)
(n) x y = Intersection bmL x y

(--) infixr 2 :: (SetTerm a) (SetTerm a) -> (Term a)
(--) x y = Difference bmL x y

term = 	
	x =. new //:.
	//x =. add (int 6 * (z = . int 7)) (setv x) :.
	//y =. setv x U setv x :.
	//While (card (setv x) <. int 5)
	//(x =. add (card (setv x)) (setv x)) :.
	//z =. setv x -- ((setv x) n (add (elem z) new))

x = "x";
y = "y";
z = "z";

Start = 42