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
    toSpec (DPt output state) 	= [Pt [output] state]
    toSpec (DFt f) 				= [Ft (map f)]

// 3 A Vending Machine Model
:: In = Choice Product | InsertCoin | Help
:: Out = Out Product | ReturnCoin | Info String
:: Product = Coffee | Tea | CandyBar

:: State = S Int

spec :: State In 				-> DTrans Out State
spec (S 1) (Choice Tea) 		= DPt (Out Tea) (S 0)
spec (S 2) (Choice Coffee) 		= DPt (Out Coffee) (S 0)
spec (S 3) (Choice CandyBar)	= DPt (Out CandyBar) (S 0)
spec (S 0) InsertCoin 			= DPt (Info "") (S 1)
spec (S 1) InsertCoin 			= DPt (Info "") (S 2)
spec (S 2) InsertCoin 			= DPt (Info "") (S 3)
spec (S 3) InsertCoin 			= DPt ReturnCoin (S 3)
spec s Help 					= DPt (Info "") s
spec s b 						= DPt (Info "") s

value p = case p of
	Tea = 1
	Coffee = 2
	CandyBar = 3
	
sut :: State In 			-> ([Out],State)
sut (S 1) (Choice Tea) 		= ([(Out Tea)], (S 0))
sut (S 2) (Choice Coffee) 	= ([(Out Coffee)], (S 0))
sut (S 3) (Choice CandyBar)	= ([(Out CandyBar)], (S 0))
sut (S 0) InsertCoin 		= ([(Info "")], (S 1))
sut (S 1) InsertCoin 		= ([(Info "")], (S 2))
sut (S 2) InsertCoin 		= ([(Info "")], (S 3))
sut (S 3) InsertCoin 		= ([ReturnCoin],(S 3))
sut s Help 					= ([(Info "")], s)
sut s b 					= ([(Info "")], s)

t0 = (S 0)
s0 = (S 0)

Start :: w -> a | gEq{|*|}, genShow{|*|} w
Start w = testConfSM [Shrink True] (trans spec) s0 sut t0 (\_.t0) w
