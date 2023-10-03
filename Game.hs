-- Game.hs: defines the functions for the guessing game
-- Author : Tristan Thomas
-- Date   : 27-9-23


module Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (ord)
import Data.List (permutations, minimumBy)
import Data.Ord (comparing)

type Location = (Char, Int)
type GameState = [[Location]]
toLocation :: String -> Maybe Location
toLocation loc
    | [x, y] <- loc, x `elem` ['A'..'D'] , y `elem` ['1'..'3'] = Just (x, read [y]::Int)
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
initialGuess = ([('A', 1), ('B', 2), ('C', 3)], state)
    where state = combinations 3 allTargets

allTargets :: [(Char, Int)]
allTargets = [(x, y) | x <- ['A'..'D'], y <- [1..3]]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = combinations k xs ++ map (x:) (combinations (k-1) xs)

nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)--[(Double,[Location])]--([Location], GameState)
nextGuess (prevGuess, prevState) feedback = (newGuess, newState)--avgLengths
    where 
        newState = pareTargets feedback prevState prevGuess
        -- for each potential target (pretend this one is correct) find the average number of potential targets remaining if any of the other potential targets (besides this one) are chosen as the next guess, which will result in the most likely correct guess.
        avgLengths = map (\x -> (avgTargets (deleteTarget x newState) x, x)) newState
        (_,newGuess) = minimumBy (comparing fst) avgLengths

-- nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)--[(Double,[Location])]--([Location], GameState)
-- nextGuess (prevGuess, prevState) feedback = (newGuess, newState)--avgLengths
--     where 
--         newState = pareTargets feedback prevState prevGuess
--         -- for each potential target (pretend this one is correct) find the average number of potential targets remaining if any of the other potential targets (besides this one) are chosen as the next guess, which will result in the most likely correct guess.
--         --avgLengths = map (\x -> (avgTargets (deleteTarget x newState) x, x)) newState
--         newGuess = head newState

-- finds if the potential target is consistent with the previous feedback
consistent :: (Int,Int,Int) -> [Location] -> [Location] -> Bool
consistent prevAns potentialGuess prevGuess =  newAns == prevAns
    where newAns = feedback prevGuess potentialGuess


avgTargets :: GameState -> [Location] -> Double
avgTargets state potTarget = average remainings
    where 
        remainings = map calculateRemaining state

        calculateRemaining :: [Location] -> Double
        -- finds the potential targets remaining for any two potential targets (x and potTarget)
        calculateRemaining x = fromIntegral $ length $ pareTargets answer state potTarget
            where answer = feedback potTarget x


pareTargets :: (Int,Int,Int) -> GameState -> [Location] -> GameState
pareTargets feedback state guess  = filter (consistent feedback guess) state

average :: Fractional a => [a] -> a
average xs = sum xs / fromIntegral (length xs)
-- the remaining guess (n - 1 of them) and the potential guess (1 of them) are only needed to caluclate the feedback, filtering can be done through feedback, potential guess and state (all other targets)

equalTarget :: [Location] -> [Location] -> Bool
equalTarget xs ys = xs `elem` permutations ys

-- searchTarget :: [Location] -> GameState -> Bool
-- searchTarget _ [] = False
-- searchTarget xs (y:ys) = equalTarget xs y || searchTarget xs ys

deleteTarget :: [Location] -> GameState -> GameState
deleteTarget x = filter $ not . equalTarget x

