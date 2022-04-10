module Main where

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

data File = A | B | C | D | E | F | G | H

type Coord = (File, Int)

newtype Board = Board [[Maybe Piece]]

instance Show Board where
    show (Board b) = showBoard b where
        showBoard :: [[Maybe Piece]] -> String
        showBoard xs
            | length xs == length b = "  a b c d e f g h  \n" ++ showRank (length xs) (head xs) ++ showBoard (tail xs)
            | null xs               = "  a b c d e f g h  "
            | otherwise = showRank (length xs) (head xs) ++ showBoard (tail xs)

        showRank :: Int -> [Maybe Piece] -> String
        showRank n rs = show n ++ " " ++ unwords (map showPiece rs) ++ " " ++ show n ++ "\n"

showPiece :: Maybe Piece -> String
showPiece (Just a) = show a
showPiece Nothing  = "_"

tupleFromCoord :: Coord -> (Int, Int)
tupleFromCoord (file, rank) = (row, col) where
    row = 8 - rank
    col = case file of
        A -> 0
        B -> 1
        C -> 2
        D -> 3
        E -> 4
        F -> 5
        G -> 6
        H -> 7

coordFromTuple :: (Int, Int) -> Coord
coordFromTuple (row, col) = (file, rank) where
    rank = 8 - row
    file = case col of
        0 -> A
        1 -> B
        2 -> C
        3 -> D
        4 -> E
        5 -> F
        6 -> G
        7 -> H
        _ -> error "invalid file"

pieceAt :: Coord -> Board -> Maybe Piece
pieceAt c (Board b) = b !! row !! col where (row, col) = tupleFromCoord c

editSlot :: Coord -> Maybe Piece -> Board -> Board
editSlot c p (Board b) = Board (ub ++ (lr ++ p : rr) : lb) where
    (ub, nrow : lb) = splitAt row b
    (lr, ncol : rr) = splitAt col nrow
    (row, col)      = tupleFromCoord c

moveSlot :: Coord -> Coord -> Board -> Board
moveSlot c1 c2 b = editSlot c1 Nothing $ editSlot c2 (pieceAt c1 b) b where

blankBoard :: Board
blankBoard = Board $ replicate 8 $ replicate 8 Nothing

startBoard :: Board
startBoard =
    -- Black major pieces
    editSlot (A,8) (Just $ Rook Black) $ editSlot (B,8) (Just $ Knight Black) $ editSlot (C,8) (Just $ Bishop Black) $ editSlot (D,8) (Just $ Queen Black) $
    editSlot (E,8) (Just $ King Black) $ editSlot (F,8) (Just $ Bishop Black) $ editSlot (G,8) (Just $ Knight Black) $ editSlot (H,8) (Just $ Rook  Black) $
    -- Black pawns
    editSlot (A,7) (Just $ Pawn Black) $ editSlot (B,7) (Just $ Pawn   Black) $ editSlot (C,7) (Just $ Pawn   Black) $ editSlot (D,7) (Just $ Pawn  Black) $
    editSlot (E,7) (Just $ Pawn Black) $ editSlot (F,7) (Just $ Pawn   Black) $ editSlot (G,7) (Just $ Pawn   Black) $ editSlot (H,7) (Just $ Pawn  Black) $
    -- White major pieces
    editSlot (A,1) (Just $ Rook White) $ editSlot (B,1) (Just $ Knight White) $ editSlot (C,1) (Just $ Bishop White) $ editSlot (D,1) (Just $ Queen White) $
    editSlot (E,1) (Just $ King White) $ editSlot (F,1) (Just $ Bishop White) $ editSlot (G,1) (Just $ Knight White) $ editSlot (H,1) (Just $ Rook  White) $
    -- White pawns
    editSlot (A,2) (Just $ Pawn White) $ editSlot (B,2) (Just $ Pawn   White) $ editSlot (C,2) (Just $ Pawn   White) $ editSlot (D,2) (Just $ Pawn  White) $
    editSlot (E,2) (Just $ Pawn White) $ editSlot (F,2) (Just $ Pawn   White) $ editSlot (G,2) (Just $ Pawn   White) $ editSlot (H,2) (Just $ Pawn  White)

    blankBoard

isOfType :: Maybe Piece -> (Color -> Piece) -> Bool
isOfType p t = p == Just (t White) || p == Just (t Black)

isMoveLegal :: Coord -> Coord -> Board -> Bool
isMoveLegal c1 c2 b
    | any (< 0) [row, col, row2, col2] || any (> 7) [row, col, row2, col2] = False
    | piece `isOfType` Rook      && canRookMove      = True
    | piece `isOfType` Bishop    && canBishopMove    = True
    | piece `isOfType` Queen     && canQueenMove     = True
    | piece `isOfType` Knight    && canKnightMove    = True
    | piece `isOfType` King      && canKingMove      = True
    | piece == Just (Pawn White) && canWhitePawnMove = True
    | piece == Just (Pawn Black) && canBlackPawnMove = True
    | otherwise = False
    where
        (file, rank)   = c1
        (file2, rank2) = c2
        (row,  col)    = tupleFromCoord c1
        (row2, col2)   = tupleFromCoord c2
        piece          = pieceAt c1 b

        slotPiece :: (Int, Int) -> Maybe Piece
        slotPiece c = pieceAt (coordFromTuple c) b

        canRookMove :: Bool
        canRookMove = row == row2 || col == col2 && (row, col) /= (row2, col2)

        canBishopMove :: Bool
        canBishopMove = abs (row - row2) == abs (col - col2) && (row, col) /= (row2, col2)

        canQueenMove :: Bool
        canQueenMove = canRookMove || canBishopMove

        canKnightMove :: Bool
        canKnightMove = (abs (row2 - row) == 2 && abs (col2 - col) == 1) || (abs (row2 - row) == 1 && abs (col2 - col) == 2)

        canKingMove :: Bool
        canKingMove = abs (row2 - row) <= 1 && abs (col2 - col) <= 1 && (row, col) /= (row2, col2)

        canWhitePawnMove :: Bool
        canWhitePawnMove = (row2 - row <= (if rank == 2 && slotPiece (row, col + 1) == Nothing then (if slotPiece (row, col + 2) == Nothing then 2 else 1) else 0) && col == col2)
                         || (pieceAt c2 b /= Nothing && row2 == row + 1 && abs (col2 - col) == 1)
                         && row /= row2

        canBlackPawnMove :: Bool
        canBlackPawnMove = (row - row2 <= (if rank == 7 && slotPiece (row, col - 1) == Nothing then (if slotPiece (row, col - 2) == Nothing then 2 else 1) else 0) && col == col2)
                         || (pieceAt c2 b /= Nothing && row2 == row - 1 && abs (col2 - col) == 1)
                         && row /= row2

main :: IO ()
main = undefined
