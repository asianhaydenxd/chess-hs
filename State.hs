module State where

import Data.List
import Data.Maybe

data Color = White | Black deriving Eq

data Piece
    = Pawn   Color
    | Rook   Color
    | Knight Color
    | Bishop Color
    | Queen  Color
    | King   Color
    deriving Eq

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

type Coord = (Int, Int)

type Board = [[Maybe Piece]]

showBoard :: Board -> String
showBoard xs = showBoard' xs where
    showBoard' ys
        | length ys == length xs = "  a b c d e f g h  \n" ++ showRank (length ys) (last ys) ++ showBoard' (init ys)
        | null ys                = "  a b c d e f g h  "
        | otherwise              = showRank (length ys) (last ys) ++ showBoard' (init ys)
        
    showRank n rs = show n ++ " " ++ unwords (map showPiece rs) ++ " " ++ show n ++ "\n"

showPiece :: Maybe Piece -> String
showPiece (Just a) = show a
showPiece Nothing  = "_"

coord :: String -> Coord
coord s
    | length s /= 2 = error ""
    | not $ head s `elem` letters = error ""
    | not $ last s `elem` numbers = error ""
    | otherwise = (\f (a1, a2) -> (f a1, f a2)) fromJust (elemIndex (head s) letters, elemIndex (last s) numbers)
    where
        letters = "abcdefgh" :: String
        numbers = "12345678" :: String

pieceAt :: Coord -> Board -> Maybe Piece
pieceAt (row, col) b = b !! col !! row

editSlot :: Coord -> Maybe Piece -> Board -> Board
editSlot (file, rank) p b = (ub ++ (lr ++ p : rr) : lb) where
    (ub, nrow : lb) = splitAt rank b
    (lr, ncol : rr) = splitAt file nrow

moveSlot :: Coord -> Coord -> Board -> Board
moveSlot c1 c2 b = editSlot c1 Nothing $ editSlot c2 (pieceAt c1 b) b
