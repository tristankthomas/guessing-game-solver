-- Game.hs: defines the functions for the guessing game
-- Author : Tristan Thomas
-- Date   : 27-9-23


module Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (ord)
import Data.List (permutations)

type Location = (Char, Int)
type GameState = [[Location]]
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
compareGuess :: Location -> Location -> (Int, Int, Int)
compareGuess target guess
    | target == guess = (1,0,0)
    | adjacent 1 target guess = (0,1,0)
    | adjacent 2 target guess = (0,0,1)
    | otherwise = (0,0,0)

adjacent :: Int -> Location -> Location -> Bool
adjacent num (x1,y1) (x2,y2) = abs (ord x1 - ord x2) <= num && abs (y1 - y2) <= num

sumFeedback :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
sumFeedback (a,b,c) (d,e,f) = (a+d, b+e, c+f)

-- good solution is to add all possible guesses and then each time reduce this number based on feedback, for example if the feedback is not close 
initialGuess :: ([Location], GameState)
initialGuess = ([('A', 1), ('B', 2), ('C', 3)], state)
    where state = combinations 3 allTargets

allTargets :: [(Char, Int)]
allTargets = [(x, y) | x <- ['A'..'H'], y <- [1..4]]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = combinations k xs ++ map (x:) (combinations (k-1) xs)

nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (loc, state) feedback = (newLoc, newState)
    where 
        newLoc = head state
        newState = deleteTarget newLoc state


equalTarget :: [Location] -> [Location] -> Bool
equalTarget xs ys = xs `elem` permutations ys

searchTarget :: [Location] -> GameState -> Bool
searchTarget _ [] = False
searchTarget xs (y:ys) = equalTarget xs y || searchTarget xs ys

deleteTarget :: [Location] -> GameState -> GameState
deleteTarget x = filter $ not . equalTarget x

