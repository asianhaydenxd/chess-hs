module Main where

import Chess

playChess :: IO ()
playChess = play [] where
    play :: Game -> IO ()
    play = undefined

main :: IO ()
main = playChess