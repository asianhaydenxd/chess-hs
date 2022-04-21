module Main where

import Chess

inputToMove :: Color -> String -> Move
inputToMove c s
    | areValidCoords s = Move (file, rank) (file2, rank2) detectPromotion
    | s == "0-0-0"     = if c == White then WhiteLongCastle  else BlackLongCastle
    | s == "0-0"       = if c == White then WhiteShortCastle else BlackShortCastle
    | otherwise        = error "Invalid input"
    where
        areValidCoords s = ((length s == 4) && (s !! 0 `elem` letters) && (s !! 1 `elem` numbers) && (s !! 2 `elem` letters) && (s !! 3 `elem` numbers)) || isPromotion
        isPromotion = (length s == 6) && (areValidCoords (take 4 s)) && (last (init s) == '=') && (last s `elem` promoteLetters)

        (file,  rank ) = (coord $ take 2 s)
        (file2, rank2) = (coord $ take 2 $ drop 2 s)

        detectPromotion
            | not $ (c == White && rank2 == 7) || (c == Black && rank2 == 0) = NoPromotion
            | isPromotion = Promotion $ pieceToConst (last s) c
            | otherwise   = Promotion $ Queen c

        letters = "abcdefgh"
        numbers = "12345678"
        promoteLetters = "RNBQ"

        pieceToConst 'R' = Rook
        pieceToConst 'N' = Knight
        pieceToConst 'B' = Bishop
        pieceToConst 'Q' = Queen

playChess :: IO ()
playChess = play [] where
    play :: Game -> IO ()
    play = undefined

main :: IO ()
main = playChess