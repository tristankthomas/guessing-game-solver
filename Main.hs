--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for guessing game project


module Main where

import System.Exit
import Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess)

testCase = "A3 C3 B1"

-- | Main code to test guessing game implementation.
main :: IO ()
main = do
    case mapM toLocation $ words testCase of
        Just target@[_,_,_] ->
            gametest target
        _ -> do
            putStrLn $ "toLocation Failed to convert one of " ++ testCase
                 ++ " to a Location"
            exitFailure


-- | Guess the given target, counting and showing the guesses.
gametest :: [Location] -> IO ()
gametest target = do
    putStrLn $ "Searching for target " ++ showLocations target
    let (guess,other) = initialGuess
    loop target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Location] -> [Location] -> Game.GameState -> Int -> IO ()
loop target guess other guesses = do
    putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
    let answer = feedback target guess
    putStrLn $ "    My answer:  " ++ show answer
    if answer == (3,0,0)
        then do
            putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
        else do
            let (guess',other') = nextGuess (guess,other) answer
            loop target guess' other' (guesses+1)


showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)
