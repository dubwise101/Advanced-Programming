module assignment06

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
	| Oper			Element Op 	Element
	| Assign		Ident 	Expression
:: Op		= +. | -. | *.
:: Set 		:== Expression
:: Element 	:== Expression
:: Ident	:== String

// 1. State
:: Val = I Int | S [Val]
:: Binding :== [(Ident,Val)]

:: State = ErrorMessage String | Bindings Binding

// 2. State Manipulations
:: Result :== (Val,State)
:: Sem :== State -> Result

rtrn :: Val -> Sem
rtrn v = \s . (v,s)

store :: Ident Val State -> Result
store i v (Bindings b) = (v,Bindings (b++[(i,v)]))
  
read :: Ident State -> Result
read i (Bindings b) = findVal i b
where
	findVal :: Ident Binding -> Result
	findVal i [(ident,val):r]
		| i == ident   	= (val,(Bindings b))
		| otherwise 	= findVal i r

fail :: String -> Sem
fail f = \s . (I 1,(ErrorMessage f)) 

(>>-) infixl 1 :: Sem (Val->Sem) -> Sem
(>>-) f g = \s1 . let (a,s2) = f s1 in g a s2	 

// 3. Evaluator
eval :: Expression -> Sem
eval New = rtrn (S [])
//eval (Insert e s) = 
//eval (Delete e s) =
eval (Variable v) = \s . read v s
//eval (Union s1 s2) =  
//eval (Difference s1 s2) = 
//eval (Intersection s1 s2) =
eval (Integer i) = rtrn (I i)
//eval (Size s) =
eval (Oper e1 +. e2) = 
	eval e1 >>- \(I n).
	eval e2 >>- \(I m). rtrn (I (n + m))
eval (Oper e1 -. e2) = 
	eval e1 >>- \(I n).
	eval e2 >>- \(I m). rtrn (I (n - m)) 
eval (Oper e1 *. e2) = 
	eval e1 >>- \(I n).
	eval e2 >>- \(I m). rtrn (I (n * m))
//eval (Assign i e) =

Start = eval New