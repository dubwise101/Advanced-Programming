module assignment9b

import gast

// Maarten Derks s4191552

:: Matcher a :== a -> Results
:: Results :== (Bool,[String])

Is :: (Matcher a) a -> Results
Is a b = a b

EqualTo :: a a -> Results | gEq{|*|}, genShow{|*|} a	
EqualTo	a b	= (a === b,	["EqualTo",show1 a])	
  
LessThan :: a a -> Results | gLess{|*|}, genShow{|*|} a	
LessThan a b = (a -<- b, ["LessThan",show1 a])	

Not :: (Matcher a) a -> Results
Not m v = inverseBool (m v)
    where
    inverseBool :: Results -> Results
    inverseBool (True,s) = (False,s)
    inverseBool (False,s) = (True,s)
  
Either :: (Matcher a) (Matcher a) a -> Results
Either f g x = case f x of
				(True,a)	= (True,a)
				otherwise	= g x						
				
Contains :: a [a] -> Results | gEq{|*|}, genShow{|*|} a	
Contains a [] = (False,[])
Contains a b = (testMember a b,[])	
where 
	testMember:: a [a] -> Bool | gEq{|*|} a
	testMember x [hd:tl] = hd===x || testMember x tl
	testMember x []	= False		
	
ContainsString :: String String -> Results	
ContainsString a b = (is_substring a b,[])

is_equal :: String String -> Bool
is_equal a b = size a == size b && same_chars (size a-1)
where
	same_chars :: Int -> Bool
	same_chars i	= i < 0 || a.[i] == b.[i] && same_chars (i-1)

is_substring :: String String -> Bool
is_substring a b = size a <= size b && is_substring_from (size b-size a)
where
	is_substring_from :: Int -> Bool
	is_substring_from i
	| i < 0			= False
	| otherwise		= is_equal a (b%(i,i+size a-1)) || is_substring_from (i-1)

AssertThat :: String a (Matcher a) -> Results
AssertThat s x g = case g x of
					(True,a) = (True,a)
					(False,_) = (False,[s])

doTest :: [Results] -> [String]
doTest f = makeReport f 0 0

makeReport :: [Results] Int Int -> [String]
makeReport [] s f = ["\npasses = " +++ toString s +++ ", fails = " +++ toString f]
makeReport [(True,message):n] s f = makeReport n (s+1) f
makeReport [(_,message):n] s f = ["\n" +++ toString (f+1) +++ ": ":message] ++ makeReport n s (f+1)

a1 = AssertThat "(2*2) (Is (EqualTo (2+2)))" (2*2) (Is (EqualTo (2+2)))
a2 = AssertThat "(3*3) (EqualTo (3+3))" (3*3) (EqualTo (3+3)) // fail
a3 = AssertThat "(length [0..3]) is not 4" (length [0..3] ) (Not (EqualTo 4)) // fail
a4 = AssertThat "[0..3] (Contains 2)" [0..3] (Contains 2)
a5 = AssertThat "[0..3] (Contains 7)" [0..3] (Contains 7) // fail
a6 = AssertThat "[0..3] (Either (EqualTo [1]) (Contains 7))" [0..3] (Either (EqualTo [1] ) (Contains 7)) // fail
a7 = AssertThat "\"hello world\" (ContainsString \"hello \")" "hello world" (ContainsString "hello")
a8 = AssertThat "\"hello world\" (ContainsString \"world \")" "hello world" (ContainsString "world")
a9 = AssertThat "\"Red, yellow and blue \"" "Who is afraid of red, yellow and blue" (ContainsString "Red") // fail

Start = doTest [a1,a2,a3,a4,a5,a6,a7,a8,a9]

/*
["
1: ","(3*3) (EqualTo (3+3))","
2: ","(length [0..3]) is not 4","
3: ","[0..3] (Contains 7)","
4: ","[0..3] (Either (EqualTo [1]) (Contains 7))","
5: ",""Red, yellow and blue "","
passes = 4, fails = 5"]
*/
