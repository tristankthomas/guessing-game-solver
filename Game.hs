-- Game.hs: defines the functions for the guessing game
-- Author : Tristan Thomas
-- Date   : 27-9-23

module Game (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

type Location = (Char, Int)
type GameState = ()
toLocation :: String -> Maybe Location
fromLocation :: Location -> String
feedback :: [Location] -> [Location] -> (Int,Int,Int)
initialGuess :: ([Location], GameState)
nextGuess :: ([Location], GameState) -> (Int,Int,Int) -> ([Location], GameState)