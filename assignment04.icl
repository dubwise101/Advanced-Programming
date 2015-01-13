module assignment04

/*
	Advanced Programming.
	Skeleton for assignment 4.
	To be used in a project with the environment iTasks.
	Pieter Koopman, pieter@cs.ru.nl

	Make sure the "iTasks-SDK" folder is in one of the search locations of the executable:
	.
	..
	..\..
	..\..\..
	..\..\..\..
	..\..\..\..\..
	C:\Clean 2.4
	C:\Program Files
	A convenient way to do this is putting this project in a (sub)folder 
	of iTask-SDK in the Clean 2.4 folder.
	
	You can also use the -sdk commandline flag to set the path.
	Example: -sdk C:\Users\johndoe\Desktop\Clean2.4\iTasks-SDK
*/

import iTasks
import Math.Random
:: Idea = { title :: String, description :: Maybe Note }
:: Name	:== String

:: NamedIdea = { id :: Int, name :: Name, idea :: Idea}

derive class iTask Idea
derive class iTask NamedIdea	// generic magic

doIdentified :: (Name -> Task x) -> Task x | iTask x
doIdentified task
	=   enterInformation "Enter your name" []
	>>= task

editIdea :: Name -> Task [NamedIdea]
editIdea name
	= enterInformation (name +++ " add your ideas") [EnterWith (map (\idea -> {NamedIdea | id=hd (genRandInt 42),name=name,idea=idea}))]

Start :: *World -> *World
Start world
	= startEngine
		(	doIdentified editIdea
		>>=	viewInformation "The result" []
		)
		world