module assignment5

import iTasks

// Maarten Derks s4191552

:: IdeaData = { title :: String, description :: Maybe Note}
:: Name	:== String

:: Idea = { id :: Int, name :: Name, idea :: IdeaData, likes :: Int}

derive class iTask IdeaData
derive class iTask Idea

ideas :: Shared [Idea]
ideas = sharedStore "Ideas" []

addNewIdea :: Name-> Task [Idea]
addNewIdea name = (enterInformation ("Add an idea") []              
              	>>= \idea . upd (\ideas -> ideas++[{id=length ideas, name = name, idea=idea, likes=0}]) ideas
              	)
				-||-
				(viewIdeas
                >>* [ OnAction (Action "Ok" [])     	(always (upd (\i -> i) ideas)) 
                   	, OnAction (Action "Delete" [])   	(ifValue (isMine name) delete)
                   	, OnAction (Action "Like" [])  		(ifValue (notMine name) like)
                   	, OnAction (Action "Delete All" []) (always (set [] ideas))])           
           
viewIdeas :: Task Idea
viewIdeas = enterChoiceWithShared "List so far.." [] ideas
		 >&^ viewSelected
where viewSelected ideas = viewSharedInformation "Selected idea" [] ideas
		
doIdentified :: Task Name
doIdentified = enterInformation "Enter your name" []

delete :: Idea -> Task [Idea]
delete idea = upd (\id = removeAt (indexOf idea id) id) ideas

addLike :: Idea -> Idea
addLike {name = u, idea = i, id = n, likes =l} = {name = u, idea = i, id = n, likes = (l+1)}

like :: Idea -> Task [Idea]
like idea = upd (\id = updateAt (indexOf idea id) (addLike idea) id) ideas

indexOf :: Idea [Idea] -> Int
indexOf idea [b:c]
       | idea == b = 0
       | otherwise = 1 + (indexOf idea c)
       
instance == Idea where
       (==) {id = a, name = _, idea = _} {id = b, name = _, idea = _} = a==b
       
notMine :: Name Idea -> Bool
notMine a b = not (isMine a b)

isMine :: Name Idea -> Bool
isMine a {name = b, idea = i, id = n, likes =l} = a==b

Start :: *World -> *World
Start world
	= startEngine 
		(	
			doIdentified
			>>= \name -> forever (addNewIdea name)
		)
		world