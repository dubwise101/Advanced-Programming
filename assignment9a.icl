module assignment9a

import gast

// Maarten Derks s4191552

:: Matcher a
	= Is 		(Matcher a)
	| EqualTo	a
	| LessThan 	a
	| Not 		(Matcher a)
	| Either 	(Matcher a)	(Matcher a)

:: TestFun :== UnitTest -> (UnitTest->[String]) -> [String]
:: UnitTest = { fail :: Int , succ :: Int }

AssertThat :: String a (Matcher a) -> TestFun | gEq{|*|}, genShow{|*|}, gLess{|*|} a
AssertThat s x (Is y) = AssertThat s x y
AssertThat s x (EqualTo y)  
	= \state c . if (x === y)
    (c { state & succ = state.succ+1 })
    ([toString (state.fail + 1) +++ ": " +++ s +++ "\n": c { state & fail = state.fail + 1 }]) 
AssertThat s x (LessThan y) 
	= \state c . if (x -<- y)
    (c { state & succ = state.succ+1 })
    ([toString (state.fail + 1) +++ ": " +++ s +++ "\n": c { state & fail = state.fail + 1 }])
AssertThat s x (Not (Is y)) = AssertThat s x (Not y)
AssertThat s x (Not (EqualTo y)) = AssertThat s x (LessThan y)
AssertThat s x (Not (LessThan y)) = AssertThat s x (EqualTo y)
AssertThat s x (Not (Not y)) = AssertThat s x y
AssertThat s x (Not (Either y z)) = AssertThat s x (Either (Not y) (Not z))
AssertThat s x (Either y z) = case ((AssertThat s x y){ fail = 0 , succ = 0 } checkFail) of
								["1"] = AssertThat s x z
								otherwise = AssertThat s x y

checkFail :: UnitTest -> [String]
checkFail { fail = 1 , succ = 0 } 	= ["1"]
checkFail _ 						= []			

doTest :: TestFun -> [String]
doTest f = f { fail = 0, succ = 0 } makeReport

makeReport :: UnitTest -> [String]
makeReport {fail, succ} = ["passes = " +++ toString succ +++ ", fails = " +++ toString fail]

instance * TestFun where
	(*) t1 t2 = \state cont . t1 state (\state2 . t2 state2 cont)

a1 = AssertThat "(2*2) (Is (EqualTo (2+2)))" (2*2) (Is (EqualTo (2+2)))
a2 = AssertThat "(3*3) (EqualTo (3+3))" (3*3) (EqualTo (3+3)) // fail
a3 = AssertThat "(length [0..3]) is not 4" (length [0..3] ) (Not (EqualTo 4)) // fail

Start = doTest (a1 * a2 * a3)