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

tupleFromCoords :: String -> Maybe (Int, Int)
tupleFromCoords xs =
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

editSlot :: (Int, Int) -> Maybe Piece -> Board -> Board
editSlot (r,f) p (Board xs) = Board (lb ++ (ls ++ p : rs) : rb) where
    (lb, rn:rb) = splitAt r xs
    (ls, fn:rs) = splitAt f rn

moveSlot :: (Int, Int) -> (Int, Int) -> Board -> Board
moveSlot (r,f) c2 (Board b) = editSlot (r,f) Nothing $ editSlot c2 (b !! r !! f) (Board b)

blankBoard :: Board
blankBoard = Board $ replicate 8 $ replicate 8 Nothing

startBoard :: Board
startBoard =
    -- Black major pieces
    editSlot (0,0) (Just $ Rook Black) $ editSlot (0,1) (Just $ Knight Black) $ editSlot (0,2) (Just $ Bishop Black) $ editSlot (0,3) (Just $ Queen Black) $
    editSlot (0,4) (Just $ King Black) $ editSlot (0,5) (Just $ Bishop Black) $ editSlot (0,6) (Just $ Knight Black) $ editSlot (0,7) (Just $ Rook  Black) $
    -- Black pawns
    editSlot (1,0) (Just $ Pawn Black) $ editSlot (1,1) (Just $ Pawn   Black) $ editSlot (1,2) (Just $ Pawn   Black) $ editSlot (1,3) (Just $ Pawn  Black) $
    editSlot (1,4) (Just $ Pawn Black) $ editSlot (1,5) (Just $ Pawn   Black) $ editSlot (1,6) (Just $ Pawn   Black) $ editSlot (1,7) (Just $ Pawn  Black) $
    -- White major pieces
    editSlot (7,0) (Just $ Rook White) $ editSlot (7,1) (Just $ Knight White) $ editSlot (7,2) (Just $ Bishop White) $ editSlot (7,3) (Just $ Queen White) $
    editSlot (7,4) (Just $ King White) $ editSlot (7,5) (Just $ Bishop White) $ editSlot (7,6) (Just $ Knight White) $ editSlot (7,7) (Just $ Rook  White) $
    -- White pawns
    editSlot (6,0) (Just $ Pawn White) $ editSlot (6,1) (Just $ Pawn   White) $ editSlot (6,2) (Just $ Pawn   White) $ editSlot (6,3) (Just $ Pawn  White) $
    editSlot (6,4) (Just $ Pawn White) $ editSlot (6,5) (Just $ Pawn   White) $ editSlot (6,6) (Just $ Pawn   White) $ editSlot (6,7) (Just $ Pawn  White) $

    blankBoard

main :: IO ()
main = undefined
