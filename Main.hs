module Main where

import Data.List
import Data.Maybe

data Color
    = White
    | Black

data Piece
    = Pawn   Color
    | Rook   Color
    | Knight Color
    | Bishop Color
    | Queen  Color
    | King   Color

instance Show Piece where
    show (Pawn   White) = "♙"
    show (Rook   White) = "♖"
    show (Knight White) = "♘"
    show (Bishop White) = "♗"
    show (Queen  White) = "♕"
    show (King   White) = "♔"

    show (Pawn   Black) = "♟"
    show (Rook   Black) = "♜"
    show (Knight Black) = "♞"
    show (Bishop Black) = "♝"
    show (Queen  Black) = "♛"
    show (King   Black) = "♚"

newtype Board = Board [[Maybe Piece]]

instance Show Board where
    show (Board b) = showBoard b where
        showBoard :: [[Maybe Piece]] -> String
        showBoard xs
            | length xs == length b = "  a b c d e f g h  \n" ++ showRank (length xs) (head xs) ++ showBoard (tail xs)
            | length xs == 0        = "  a b c d e f g h  "
            | otherwise = showRank (length xs) (head xs) ++ showBoard (tail xs)

        showRank :: Int -> [Maybe Piece] -> String
        showRank n rs = show n ++ " " ++ unwords (map showPiece rs) ++ " " ++ show n ++ "\n"

fileLetters :: String
fileLetters = "abcdefgh"

rankNumbers :: String
rankNumbers = "87654321"

showPiece :: Maybe Piece -> String
showPiece (Just a) = show a
showPiece Nothing  = "_"

tupleFromRank :: String -> Maybe (Int, Int)
tupleFromRank xs =
    case maybeFromRank xs of
        Nothing               -> Nothing
        Just (Nothing, _)     -> Nothing
        Just (_, Nothing)     -> Nothing
        Just (Just a, Just b) -> Just (a, b)
    where
        maybeFromRank :: String -> Maybe (Maybe Int, Maybe Int)
        maybeFromRank ys
            | length ys /= 2 = Nothing
            | otherwise = Just (elemIndex (last ys) rankNumbers, elemIndex (head ys) fileLetters)

editSlot :: Board -> (Int, Int) -> Maybe Piece -> Board
editSlot (Board b) (r,f) p =
    let (lb,rn:rb) = splitAt r b
        (lr,fn:rr) = splitAt f rn
    in Board (lb ++ (lr ++ p : rr) : rb)

blankBoard :: Board
blankBoard = Board $ replicate 8 $ replicate 8 $ Nothing

stringToBoard :: String -> Board
stringToBoard xs = Board $ map (\Piece) (lines xs)

startBoard :: Board
startBoard = unlines [ "♜♞♝♝♞♜"
                     , "♟♟♟♟♟♟♟♟"
                     ]

main :: IO ()
main = undefined