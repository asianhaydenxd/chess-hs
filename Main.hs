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

isWhite :: Maybe Piece -> Bool
isWhite (Just (Pawn   White)) = True
isWhite (Just (Rook   White)) = True
isWhite (Just (Knight White)) = True
isWhite (Just (Bishop White)) = True
isWhite (Just (Queen  White)) = True
isWhite (Just (King   White)) = True
isWhite _                     = False

isBlack :: Maybe Piece -> Bool
isBlack (Just (Pawn   Black)) = True
isBlack (Just (Rook   Black)) = True
isBlack (Just (Knight Black)) = True
isBlack (Just (Bishop Black)) = True
isBlack (Just (Queen  Black)) = True
isBlack (Just (King   Black)) = True
isBlack _                     = False

isMoveLegal :: Coord -> Coord -> Board -> Bool
isMoveLegal (file, rank) (file2, rank2) b
    | any (< 0) [file, rank, file2, rank2] || any (> 7) [file, rank, file2, rank2] = False
    | piece `isOfType` Rook      && canRookMove      = True
    | piece `isOfType` Bishop    && canBishopMove    = True
    | piece `isOfType` Queen     && canQueenMove     = True
    | piece `isOfType` Knight    && canKnightMove    = True
    | piece `isOfType` King      && canKingMove      = True
    | piece == Just (Pawn White) && canWhitePawnMove = True
    | piece == Just (Pawn Black) && canBlackPawnMove = True
    | otherwise = False
    where
        piece = pieceAt (file, rank) b

        canRookMove :: Bool
        canRookMove = (file == file2 || rank == rank2) && (file, rank) /= (file2, rank2)

        canBishopMove :: Bool
        canBishopMove = abs (file - file2) == abs (rank - rank2) && (file, rank) /= (file2, rank2)

        canQueenMove :: Bool
        canQueenMove = canRookMove || canBishopMove

        canKnightMove :: Bool
        canKnightMove = (abs (file2 - file) == 2 && abs (rank2 - rank) == 1) || (abs (file2 - file) == 1 && abs (rank2 - rank) == 2)

        canKingMove :: Bool
        canKingMove = abs (file2 - file) <= 1 && abs (rank2 - rank) <= 1 && (file, rank) /= (file2, rank2)

        canWhitePawnMove :: Bool
        canWhitePawnMove
            | file == file2 && rank == 1 && rank2 == 3 && pieceAt (file, 2) b == Nothing && pieceAt (file, 3) b == Nothing = True
            | file == file2 && rank2 == rank + 1 && pieceAt (file, rank + 1) b == Nothing                                  = True
            | file2 == file + 1 && rank2 == rank + 1 && isBlack (pieceAt (file + 1, rank + 1) b)                           = True
            | file2 == file - 1 && rank2 == rank + 1 && isBlack (pieceAt (file - 1, rank + 1) b)                           = True
            | otherwise                                                                                                    = False
        
        canBlackPawnMove :: Bool
        canBlackPawnMove
            | file == file2 && rank == 6 && rank2 == 4 && pieceAt (file, 5) b == Nothing && pieceAt (file, 4) b == Nothing = True
            | file == file2 && rank2 == rank - 1 && pieceAt (file, rank - 1) b == Nothing                                  = True
            | file2 == file + 1 && rank2 == rank - 1 && isWhite (pieceAt (file + 1, rank - 1) b)                           = True
            | file2 == file - 1 && rank2 == rank - 1 && isWhite (pieceAt (file - 1, rank - 1) b)                           = True
            | otherwise                                                                                                    = False

isClear :: Coord -> Coord -> Board -> Bool
isClear (x, y) (x2, y2) b
    -- Straight lines
    | x2 == x && y2 < y = isClear' (x2, y2) ( 0,  1)
    | x2 == x && y2 > y = isClear' (x2, y2) ( 0, -1)
    | y2 == y && x2 < x = isClear' (x2, y2) ( 1,  0)
    | y2 == y && x2 > x = isClear' (x2, y2) (-1,  0)
    -- Perfect Diagonal Check
    | abs (x2 - x) /= abs (y2 - y) = error "not a valid diagonal"
    -- Diagonals
    | x2 >  x && y2 > y = isClear' (x2, y2) (-1, -1)
    | x2 <  x && y2 > y = isClear' (x2, y2) ( 1, -1)
    | x2 >  x && y2 < y = isClear' (x2, y2) (-1,  1)
    | x2 <  x && y2 < y = isClear' (x2, y2) ( 1,  1)
    -- Strange behavior
    | x2 == x && y2 == y = error "coordinates are identical"
    | otherwise          = error "undetected angle"
    where
        isClear' :: Coord -> (Int, Int) -> Bool
        isClear' (x2, y2) (xi, yi)
            | isWhite (pieceAt (x, y) b) && isWhite (pieceAt (x2, y2) b) = False -- Check if the checked piece is the same color as the original
            | isBlack (pieceAt (x, y) b) && isBlack (pieceAt (x2, y2) b) = False
            | (x, y) == (x2 + xi, y2 + yi)                               = True -- Check if the next coordinate is the original
            | pieceAt (x2 + xi, y2 + yi) b /= Nothing                    = False -- Check if the next coordinate has a piece
            | otherwise                                                  = isClear' (x2 + xi, y2 + yi) (xi, yi) -- Check next coordinate

main :: IO ()
main = undefined
