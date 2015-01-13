module assignment10

import gast

// 1 Deterministic Models
:: DSpec state input output :== state input -> DTrans output state
:: DTrans output state = DPt output state | DFt (output->state)

// 2 Transformation
trans :: (DSpec s i o) -> Spec s i o
trans d = \o s . toSpec (d o s)
where
	toSpec :: (DTrans output state) -> [Trans output state]
    toSpec (DPt o s) = [Pt [o] s]
    toSpec (DFt f) = [Ft (map f)]

// 3 A Vending Machine Model
:: State = S Int

:: In = Choice Product | InsertCoin | Help
:: Out = Out Product | ReturnCoin | Info String
:: Product = Coffee | Tea | CandyBar

spec :: State In -> DTrans Out State
spec (S 1) (Choice Tea) = DPt (Out Tea) (S 0)
spec (S 2) (Choice Coffee) = DPt (Out Coffee) (S 0)
spec (S 3) (Choice CandyBar)= DPt (Out CandyBar) (S 0)
spec (S 0) InsertCoin = DPt (Info "") (S 1)
spec (S 1) InsertCoin = DPt (Info "") (S 2)
spec (S 2) InsertCoin = DPt (Info "") (S 3)
spec (S 3) InsertCoin = DPt ReturnCoin (S 3)
spec s Help = DPt (Info "") s
spec s b = DPt (Info "") s

value p = case p of
	Tea = 1
	Coffee = 2
	CandyBar = 3
	
// 4 System Under Test
exSut :: State -> (In -> ([Out],State))
exSut s = \i . subSut i
	where
	subSut :: In -> ([Out],State)
	subSut i = sut s i

sut :: State In -> ([Out],State)
sut (S 3) InsertCoin = ([(ReturnCoin)], (S 3))
sut (S n) InsertCoin = ([(Info "")], (S (n+1)))
sut (S n) (Choice Tea) 
| n == 1 = ([(Out Tea)], (S (n-1)))
| otherwise = ([(Info "")],(S n))
sut (S n) (Choice Coffee) 
| n == 2 = ([(Out Coffee)], (S (n-2)))
| otherwise = ([(Info "")],(S n))
sut (S n) (Choice CandyBar)
| n == 3 = ([(Out CandyBar)], (S (n-3)))
| otherwise = ([(Info "")],(S n))
sut state Help = ([(Info "")], state)

derive gEq State
derive gLess State
derive genShow State
derive gEq Out
derive genShow Out
derive gEq Product
derive genShow Product
derive ggen Product
derive bimap []
derive ggen In
derive genShow In

t0 = S 0

Start w = testConfSM [Shrink True] (trans spec) (S 0) exSut t0 (\_.t0) w