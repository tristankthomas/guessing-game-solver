-- Game.hs: defines the functions for the guessing game
-- Author : Tristan Thomas
-- Date   : 27-9-23


module Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (ord)
import Data.List (minimumBy, sort, group)
import Data.Ord (comparing)

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

initialGuess :: ([Location], GameState)
initialGuess = ([('B',1), ('H',2), ('H',4)], state)
    where state = combinations 3 allTargets

allTargets :: [Location]
allTargets = [(x, y) | x <- ['A'..'H'], y <- [1..4]]

combinations :: Int -> [Location] -> [[Location]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = combinations k xs ++ map (x:) (combinations (k-1) xs)

nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (prevGuess, prevState) feedback = (newGuess, newState)
    where 
        newState = filter (isConsistent feedback prevGuess) prevState
        avgLengths = map (\x -> (avgTargets newState x, x)) newState
        (_,newGuess) = minimumBy (comparing fst) avgLengths

-- finds if the potential target is consistent with the previous feedback
isConsistent :: (Int,Int,Int) -> [Location] -> [Location] -> Bool
isConsistent prevAns potentialGuess prevGuess =  newAns == prevAns
    where newAns = feedback prevGuess potentialGuess

avgTargets :: GameState -> [Location] -> Double
avgTargets state potTarget = countSum / total
    where 
        feedbacks = map (`feedback` potTarget) state
        counts = [length fb | fb <- group (sort feedbacks)]
        countSum = fromIntegral (sum [x * x | x <- counts])
        total = fromIntegral $ length state

-- can be used to create a list of all 4960 possible moves by calling: map createCases (combinations 3 allTargets)
createCases :: [Location] -> String
createCases [a,b,c] = fromLocation a ++ " " ++ fromLocation b ++ " " ++ fromLocation c
