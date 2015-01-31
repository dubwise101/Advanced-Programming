module assignment09a

import gast

:: Matcher a = Is (Matcher a)
			| EqualTo a
			| LessThan a
			| Not (Matcher a)
			| Either (Matcher a) (Matcher a)

//:: AssertThat a = AssertThat String a (Matcher a)

AssertThat :: String a (Matcher a) -> State -> State | toString, ==, <, show a
AssertThat d x m = \s.if (eval m)
						{s & pass = s.pass + 1}
						{s & fail = s.fail + 1
							, trace = [[d, " ": show x [" ": show m ["\n"]]]
										:s.trace]
						} where
						eval :: (Matcher a) -> Bool | ==, <, show a
						eval (EqualTo y) = x == y
						eval (Is m)		= eval m
						eval (LessThan y) = x < y
						eval (Not m) 		= not (eval m)
						eval (Either a b) = eval a || eval b


//AssertThat s v m = False

:: UnitTest = { fail :: Int , succ :: Int }
:: TestFun :== UnitTest -> (UnitTest->[String]) -> [String]

instance * (State->State) where (*) a1 a2 = a1 o a2
doTest :: TestFun -> [String]
doTest f = f { fail = 0, succ = 0 } makeReport

makeReport :: UnitTest -> [String]
makeReport {fail, succ} = ["\nFinised testing: ", toString (fail + succ), " tests executed, ",toString fail, " failures, and ", toString succ, " successes.\n"]

a1 = AssertThat "(2*2) (Is (EqualTo (2+2)))" (2*2) (Is (EqualTo (2+2)))
a2 = AssertThat "(3*3) (EqualTo (3+3))" (3*3) (EqualTo (3+3)) // fail
a3 = AssertThat "(length [0..3]) is not 4" (length [0..3]) (Not (EqualTo 4)) // fail

Start = doTest (a1 * a2 * a3) 