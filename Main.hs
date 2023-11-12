--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for guessing game project


module Main where
import System.Environment
import System.Exit
import Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess, isConsistent)

import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [testCase, initGuess, fileName] -> do
            case mapM toLocation $ words testCase of
                Just target@[_,_,_] -> do
                    gametest testCase initGuess fileName target 
                _ -> do
                    putStrLn $ "toLocation Failed to convert one of " ++ testCase
                         ++ " to a Location"
                    exitFailure
        _ -> do
            putStrLn "Usage: ./Game <testCase> <initGuess> <fileName>"
            exitFailure

-- | Guess the given target, counting and showing the guesses.
gametest :: String -> String -> String -> [Location] -> IO ()
gametest tc init name target = do
    putStrLn $ "Searching for target " ++ showLocations target
    let (guess,other) = initialGuess $ map (fromMaybe ('A',0) . toLocation) (words init)
    {- code used to find initial guess that results in lowest amount of available targets (use with testing_init_guess.sh)
    let answer = feedback target guess
    let newStateLength = length $ filter (isConsistent answer guess) other
    appendFile name (tc ++ ": " ++ show newStateLength ++ "\n")
    -}
    loop tc name target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: String -> String -> [Location] -> [Location] -> Game.GameState -> Int -> IO ()
loop testCase name target guess other guesses = do
    putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess ++ "--  Remaining targets: " ++ show (length other)
    let answer = feedback target guess
    putStrLn $ "    My answer:  " ++ show answer
    if answer == (3,0,0)
        then do
            putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
            appendFile name (testCase ++ "," ++ show guesses ++ "\n")
        else do
            let (guess',other') = nextGuess (guess,other) answer
            loop testCase name target guess' other' (guesses+1)


showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)