--  File     : Main.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for guessing game project


module Main where
import System.Environment
import System.Exit
import Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess)

import Data.Maybe (fromMaybe)

-- | Main code to test guessing game implementation.
-- main :: IO ()
-- main = do
--     case mapM toLocation $ words testCase of
--         Just target@[_,_,_] ->
--             gametest target
--         _ -> do
--             putStrLn $ "toLocation Failed to convert one of " ++ testCase
--                  ++ " to a Location"
--             exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [testCase, initGuess] -> do
            case mapM toLocation $ words testCase of
                Just target@[_,_,_] -> do
                    gametest testCase initGuess target 
                _ -> do
                    putStrLn $ "toLocation Failed to convert one of " ++ testCase
                         ++ " to a Location"
                    exitFailure
        _ -> do
            putStrLn "Usage: ./your_haskell_program <test_case> <initGuess>"
            exitFailure

-- | Guess the given target, counting and showing the guesses.
gametest :: String -> String -> [Location] -> IO ()
gametest tc init target = do
    putStrLn $ "Searching for target " ++ showLocations target
    let (guess,other) = initialGuess $ map (fromMaybe ('A',0) . toLocation) (words init)
    loop tc target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: String -> [Location] -> [Location] -> Game.GameState -> Int -> IO ()
loop testCase target guess other guesses = do
    putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ showLocations guess
    let answer = feedback target guess
    putStrLn $ "    My answer:  " ++ show answer
    if answer == (3,0,0)
        then do
            putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
            appendFile "results2.txt" (testCase ++ ": " ++ show guesses ++ "\n")
        else do
            let (guess',other') = nextGuess (guess,other) answer
            loop testCase target guess' other' (guesses+1)


showLocations :: [Location] -> String
showLocations = unwords . (fromLocation <$>)