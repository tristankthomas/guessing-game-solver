-- Game.hs: defines the functions for the guessing game
-- Author : Tristan Thomas
-- Date   : 27-9-23


module Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where


import Data.Char (ord)

type Location = (Char, Int)
type GameState = ()
toLocation :: String -> Maybe Location
toLocation loc
    | [x, y] <- loc, x `elem` ['A'..'H'] , y `elem` ['1'..'4'] = Just (x, read [y]::Int)
    | otherwise = Nothing

fromLocation :: Location -> String
fromLocation (x, y) = x : show y

-- finds score for each of the three guesses and returns result
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
feedback target (g:gs) =  maximum results `sumFeedback` feedback target gs
    where results = map (`compareGuess` g) target

-- compare a single guess
compareGuess :: (Num a, Num b, Num c) => Location -> Location -> (a, b, c)
compareGuess target guess
    | target == guess = (1,0,0)
    | adjacent 1 target guess = (0,1,0)
    | adjacent 2 target guess = (0,0,1) 
    | otherwise = (0,0,0)

adjacent :: Int -> Location -> Location -> Bool
adjacent num (x1,y1) (x2,y2) = abs (ord x1 - ord x2) <= num && abs (y1 - y2) <= num

sumFeedback :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
sumFeedback (a,b,c) (d,e,f) = (a+d, b+e, c+f)


initialGuess :: ([Location], GameState)
nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)