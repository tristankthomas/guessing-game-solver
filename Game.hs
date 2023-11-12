-- |
-- Module      : Game.hs 
-- Description : Defines the functions for a battleship like guessing game. 
-- Implements logic for the game state logic, returning feedback from a guess, 
-- and selecting the next best available guess.
-- Author      : Tristan Thomas
-- Date        : 27-9-23
-- 

module Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.Char (ord)
import Data.List (minimumBy, sort, group)
import Data.Ord (comparing)

type Location = (Char, Int)
type GameState = [[Location]]

-- | Converts a String to an appropriate location type (Char, Int)
--
-- Checks if the elements of the string is in the correct format and is within the 
-- allowed ranges and if so will return a location type which is compatible with
-- other functions. If not will return Nothing.
--
-- Arguments:
-- * loc - 2 character string with first element between 'A' and 'H' and second
-- element between '1' and '4'.
--
toLocation :: String -> Maybe Location
toLocation loc
    | [x, y] <- loc, x `elem` ['A'..'H'] , y `elem` ['1'..'4'] = Just (x, read [y]::Int)
    | otherwise = Nothing


-- | Converts a location type to a string
--
-- Simply converts the elements of a location tuple to a string so can be more
-- easily printed. Assumes the location is valid.
--
fromLocation :: Location -> String
fromLocation (x, y) = x : show y


-- | Computes the appropriate feedback given a guess and a target
--
-- Iterates through each of the three guess locations and compares against the
-- target and then combines each of these responses to the overall feedback. The
-- scores are represented in tuples where the first element number of correctly
-- guessed location and the next two are the number of guesses within 1 and 2 spaces
-- of a target respectively.
--
-- Arguments:
-- * target - group of 3 locations to compare against (treat as correct)
-- * guess - group of 3 locations to get feedback on (compared against target)
--
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
-- finds the maximum for each individual guess location since only the shortest
-- distance (largest feedback tuple) is taken.
feedback target (g:gs) =  maximum results `sumFeedback` feedback target gs
    where results = map (`compareGuess` g) target


-- | Computes the feedback for a single guess against a single target location
--
-- Finds the appropriate feedback by detecting the smallest distances first and 
-- moving filtering out to (0,0,0) if the target and guess are not within 2 spaces
-- of each other.
--
-- Arguments:
-- * target - single location to compare against (treat as correct)
-- * guess - single location to get feedback on (compared against target)
--
compareGuess :: Location -> Location -> (Int, Int, Int)
compareGuess target guess
    | target == guess = (1,0,0)
    | adjacent 1 target guess = (0,1,0)
    | adjacent 2 target guess = (0,0,1)
    | otherwise = (0,0,0)


-- | Checks whether two locations are adjacent by the inputted distance
--
-- Used in the compareGuess function to find if two locations are adjacent at 
-- one or two spaces returning True if so and False if not
--
-- Arguments:
-- * num - distance (number of spaces) to check between the two locations
-- * (x1,y1) - first location to check adjacency
-- * (x2,y2) - second location to check adjacency
--
adjacent :: Int -> Location -> Location -> Bool
adjacent num (x1,y1) (x2,y2) = abs (ord x1 - ord x2) <= num && abs (y1 - y2) <= num


-- | Sums two three element tuples together
--
-- Computes the elementwise sum of 2, 3 element feedback tuples
--
sumFeedback :: (Int,Int,Int) -> (Int,Int,Int) -> (Int,Int,Int)
sumFeedback (a,b,c) (d,e,f) = (a+d, b+e, c+f)

-- | Returns the initial guess and state of the game
--
-- Returns a tuple of inital location (method of choice explained in module description)
-- and finds all possible 3 location guesses available on the 4x8 game board, resulting
-- in 4960 possibilities.
--
initialGuess :: ([Location], GameState)
initialGuess = ([('B',1), ('H',2), ('H',4)], state)
    where state = combinations 3 allTargets


-- | Returns all individual locations on the board
--
-- Finds all 32 board locations which is then used for the combinations function
--
allTargets :: [Location]
allTargets = [(x, y) | x <- ['A'..'H'], y <- [1..4]]


-- | Finds all combinations of groups of Locations
--
-- Finds all 4060 groups of 3 locations in an efficient manner, used as the initial
-- state.
--
-- Arguments:
-- * k - group size
-- * (x:xs) - current list of locations (tuples)
--
combinations :: Int -> [Location] -> [[Location]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = combinations k xs ++ map (x:) (combinations (k-1) xs)


-- | Computes the best possible next guess
--
-- Finds the best guess that can be chosen in the remaining moves, by investigating
-- each remaining move and computing their expected remaining moves and selecting 
-- the move with the lowest (most likely to prune this move space).
--
-- Arguments:
-- * (prevGuess, prevState) - previous guess and state that is used to narrow
-- down the possibilities even further
-- * feedback - previous feedback which is used to pare the possible moves by 
-- checking for consistency.
--
nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)
nextGuess (prevGuess, prevState) feedback = (newGuess, newState)
    where 
        -- keep only the possible targets which are consistent with feedback
        newState = filter (isConsistent feedback prevGuess) prevState
        -- finds the expected remaining moves for all current remaining moves
        avgLengths = map (\x -> (avgTargets newState x, x)) newState
        -- selects the guess that results in the miminum expected remaining moves
        (_,newGuess) = minimumBy (comparing fst) avgLengths


-- | Checks if a given guess is consistent with feedback
--
-- Returns True if the inputted feedback is the same as the feedback between
-- the potential guess and the prior guess (indicating consistency) otherwise False
--
-- Arguments:
-- * prevAns - previous feedback (from prevGuess) used for comparison
-- * potentialGuess - guess to be checked for consistency
-- * prevGuess - previous guess that the feedback (prevAns) is based on
--
isConsistent :: (Int,Int,Int) -> [Location] -> [Location] -> Bool
isConsistent prevAns potentialGuess prevGuess =  newAns == prevAns
    where newAns = feedback prevGuess potentialGuess


-- | Computes the average targets remaining for a potential target
--
-- Finds the average possible guesses that would be remaining from a potential target
-- by fixing this target and treating all other potential targets as correct (one 
-- by one) and finding the feedback for each of these possibilities. The feedback
-- is then grouped which can be used to determine the remaning possible targets
-- if that move was selected (no need to pare moves to work this out; can use symmetry).
-- The frequencies are then weighted and averaged in the formula below.
--
-- Arguments:
-- * state - all remaining posssible targets
-- * potTarget - potential target under consideration
--
avgTargets :: GameState -> [Location] -> Double
avgTargets state potTarget = count_sum / total
    where 
        feedbacks = map (`feedback` potTarget) state
        counts = [length fb | fb <- group (sort feedbacks)]
        count_sum = fromIntegral (sum [x * x | x <- counts])
        total = fromIntegral $ length state


-- can be used to create a list of all 4960 possible moves by calling: 
-- | Converts a 3 location test case to a string
--
-- Used to create a list of all 4960 possible moves by calling:
--      map testToString (combinations 3 allTargets)
--
testToString :: [Location] -> String
testToString [a,b,c] = fromLocation a ++ " " ++ fromLocation b ++ " " ++ fromLocation c
