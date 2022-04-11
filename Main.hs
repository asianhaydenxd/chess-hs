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

type Coord = (Int, Int)

newtype Board = Board [[Maybe Piece]]

instance Show Board where
    show (Board b) = showBoard b where
        showBoard :: [[Maybe Piece]] -> String
        showBoard xs
            | length xs == length b = "  a b c d e f g h  \n" ++ showRank (length xs) (last xs) ++ showBoard (init xs)
            | null xs               = "  a b c d e f g h  "
            | otherwise = showRank (length xs) (last xs) ++ showBoard (init xs)

        showRank :: Int -> [Maybe Piece] -> String
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
pieceAt (row, col) (Board b) = b !! col !! row

editSlot :: Coord -> Maybe Piece -> Board -> Board
editSlot (file, rank) p (Board b) = Board (ub ++ (lr ++ p : rr) : lb) where
    (ub, nrow : lb) = splitAt rank b
    (lr, ncol : rr) = splitAt file nrow

moveSlot :: Coord -> Coord -> Board -> Board
moveSlot c1 c2 b = editSlot c1 Nothing $ editSlot c2 (pieceAt c1 b) b where

blankBoard :: Board
blankBoard = Board $ replicate 8 $ replicate 8 Nothing

startBoard :: Board
startBoard =
    -- Black major pieces
    editSlot (0,7) (Just $ Rook Black) $ editSlot (1,7) (Just $ Knight Black) $ editSlot (2,7) (Just $ Bishop Black) $ editSlot (3,7) (Just $ Queen Black) $
    editSlot (4,7) (Just $ King Black) $ editSlot (5,7) (Just $ Bishop Black) $ editSlot (6,7) (Just $ Knight Black) $ editSlot (7,7) (Just $ Rook  Black) $
    -- Black pawns
    editSlot (0,6) (Just $ Pawn Black) $ editSlot (1,6) (Just $ Pawn   Black) $ editSlot (2,6) (Just $ Pawn   Black) $ editSlot (3,6) (Just $ Pawn  Black) $
    editSlot (4,6) (Just $ Pawn Black) $ editSlot (5,6) (Just $ Pawn   Black) $ editSlot (6,6) (Just $ Pawn   Black) $ editSlot (7,6) (Just $ Pawn  Black) $
    -- White major pieces
    editSlot (0,0) (Just $ Rook White) $ editSlot (1,0) (Just $ Knight White) $ editSlot (2,0) (Just $ Bishop White) $ editSlot (3,0) (Just $ Queen White) $
    editSlot (4,0) (Just $ King White) $ editSlot (5,0) (Just $ Bishop White) $ editSlot (6,0) (Just $ Knight White) $ editSlot (7,0) (Just $ Rook  White) $
    -- White pawns
    editSlot (0,1) (Just $ Pawn White) $ editSlot (1,1) (Just $ Pawn   White) $ editSlot (2,1) (Just $ Pawn   White) $ editSlot (3,1) (Just $ Pawn  White) $
    editSlot (4,1) (Just $ Pawn White) $ editSlot (5,1) (Just $ Pawn   White) $ editSlot (6,1) (Just $ Pawn   White) $ editSlot (7,1) (Just $ Pawn  White)

    blankBoard

isOfType :: Maybe Piece -> (Color -> Piece) -> Bool
isOfType p t = p == Just (t White) || p == Just (t Black)

isMoveLegal :: Coord -> Coord -> Board -> Bool
isMoveLegal (row, col) (row2, col2) b
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
        piece = pieceAt (row, col) b

        slotPiece :: (Int, Int) -> Maybe Piece
        slotPiece c = pieceAt c b

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
        canWhitePawnMove = (row2 - row <= (if col == 2 && slotPiece (row, col + 1) == Nothing then (if slotPiece (row, col + 2) == Nothing then 2 else 1) else 0) && col == col2)
                         || (pieceAt (row2, col2) b /= Nothing && row2 == row + 1 && abs (col2 - col) == 1)
                         && row /= row2

        canBlackPawnMove :: Bool
        canBlackPawnMove = (row - row2 <= (if col == 7 && slotPiece (row, col - 1) == Nothing then (if slotPiece (row, col - 2) == Nothing then 2 else 1) else 0) && col == col2)
                         || (pieceAt (row2, col2) b /= Nothing && row2 == row - 1 && abs (col2 - col) == 1)
                         && row /= row2

        -- isObstructed :: Coord -> Coord -> Bool
        -- isObstructed (x, y) (x2, y2)
        --     | x == x2 && y > y2 = isObstructed' (x, y) (x2, y2) (0, -1)
        --     | x == x2 && y < y2 = isObstructed' (x, y) (x2, y2) (0, 1)

        -- isObstructed' :: Coord -> Coord -> (Int, Int) -> Bool
        -- isObstructed' c (x, y) (xi, yi)
        --     | pieceAt (x + xi, y + yi) b /= Nothing = True
        --     | c == (x + xi, y + yi) = False
        --     | otherwise = isObstructed' c (x + xi, y + yi) (xi, yi)

main :: IO ()
main = undefined
