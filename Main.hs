--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for guessing game project


module Main where
import System.Environment
import System.Exit
import Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess)

import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [testCase] -> do
            case mapM toLocation $ words testCase of
                Just target@[_,_,_] -> do
                    gameTest target 
                _ -> do
                    putStrLn $ "toLocation Failed to convert one of " ++ testCase
                         ++ " to a Location"
                    exitFailure
        _ -> do
            putStrLn "Usage: ./Game <testCase>"
            exitFailure

-- | Guess the given target, counting and showing the guesses.
gameTest :: [Location] -> IO ()
gameTest target = do
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
            -- used for testing of program (refer to previous commit)
            --appendFile name (testCase ++ "," ++ show guesses ++ "\n")
        else do
            let (guess',other') = nextGuess (guess,other) answer
            loop target guess' other' (guesses+1)


showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)
