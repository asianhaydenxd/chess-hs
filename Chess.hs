module Chess where

import Data.List
import Data.Maybe

-- Board State Records

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
    | head s `notElem` letters = error ""
    | last s `notElem` numbers = error ""
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

-- Game Records

data Promotion
    = NoPromotion
    | Promotion Piece
    deriving Eq

data Move
    = Move Coord Coord Promotion
    | WhiteLongCastle
    | WhiteShortCastle
    | BlackLongCastle
    | BlackShortCastle
    deriving Eq

instance Show Move where
    show WhiteLongCastle  = "0-0-0"
    show BlackLongCastle  = "0-0-0"
    show WhiteShortCastle = "0-0"
    show BlackShortCastle = "0-0"
    show (Move c1 c2 pr)  = (coordString c1) ++ (coordString c2) ++ (if pr == NoPromotion then [] else "=" ++ promoteInit pr) where
        coordString (f,r) = [letters !! f, numbers !! r]
        letters = "abcdefgh"
        numbers = "12345678"

        promoteInit (Promotion (Rook   _)) = "R"
        promoteInit (Promotion (Knight _)) = "N"
        promoteInit (Promotion (Bishop _)) = "B"
        promoteInit (Promotion (Queen  _)) = "Q"
        promoteInit _ = error "Invalid promotion"

type Game = [Move]

isEnPassant :: Move -> Board -> Bool
isEnPassant (Move (f, r) (f2, r2) p) b = filesAdjacent && (matchesWhite || matchesBlack) where
    matchesWhite = r == 4 && r2 == 5 && isCurrentPiece (Pawn White) && existsTargetPawn Black
    matchesBlack = r == 3 && r2 == 2 && isCurrentPiece (Pawn Black) && existsTargetPawn White

    existsTargetPawn color = pieceAt (f2, r) b == Just (Pawn color)
    isCurrentPiece p       = pieceAt (f, r) b == Just p
    filesAdjacent          = abs (f2 - f) == 1

applyMove :: Move -> Board -> Board
applyMove WhiteLongCastle            b = moveSlot (4,0) (2,0) $ moveSlot (0,0) (3,0) b
applyMove WhiteShortCastle           b = moveSlot (4,0) (6,0) $ moveSlot (7,0) (5,0) b
applyMove BlackLongCastle            b = moveSlot (4,7) (2,7) $ moveSlot (0,7) (3,7) b
applyMove BlackShortCastle           b = moveSlot (4,7) (6,7) $ moveSlot (7,7) (5,7) b
applyMove (Move c1 c2 (Promotion p)) b = editSlot c2 (Just p) $ moveSlot c1 c2 b
applyMove (Move c1 c2 (NoPromotion)) b = moveSlot c1 c2 b

gameBoard :: Game -> Board
gameBoard []     = startBoard
gameBoard (x:xs) = applyMove x (gameBoard xs)

-- Base Chess

blankBoard :: Board
blankBoard = replicate 8 $ replicate 8 Nothing

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

isMoveLegal :: Move -> Board -> Bool
isMoveLegal (Move (file, rank) (file2, rank2) pr) b
    | any (< 0) [file, rank, file2, rank2] || any (> 7) [file, rank, file2, rank2] = False
    | not $ isClear (file, rank) (file2, rank2) b    = False
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

        canRookMove   = (file == file2 || rank == rank2) && (file, rank) /= (file2, rank2)
        canBishopMove = abs (file - file2) == abs (rank - rank2) && (file, rank) /= (file2, rank2)
        canQueenMove  = canRookMove || canBishopMove
        canKnightMove = (abs (file2 - file) == 2 && abs (rank2 - rank) == 1) || (abs (file2 - file) == 1 && abs (rank2 - rank) == 2)
        canKingMove   = abs (file2 - file) <= 1 && abs (rank2 - rank) <= 1 && (file, rank) /= (file2, rank2)
        canWhitePawnMove
            | file == file2 && rank == 1 && rank2 == 3 && pieceAt (file, 2) b == Nothing && pieceAt (file, 3) b == Nothing = True
            | file == file2 && rank2 == rank + 1 && pieceAt (file, rank + 1) b == Nothing                                  = True
            | file2 == file + 1 && rank2 == rank + 1 && isBlack (pieceAt (file + 1, rank + 1) b)                           = True
            | file2 == file - 1 && rank2 == rank + 1 && isBlack (pieceAt (file - 1, rank + 1) b)                           = True
            | otherwise                                                                                                    = False
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
        isClear' (x2, y2) (xi, yi)
            | isWhite (pieceAt (x, y) b) && isWhite (pieceAt (x2, y2) b) = False -- Check if the checked piece is the same color as the original
            | isBlack (pieceAt (x, y) b) && isBlack (pieceAt (x2, y2) b) = False
            | (x, y) == (x2 + xi, y2 + yi)                               = True -- Check if the next coordinate is the original
            | pieceAt (x2 + xi, y2 + yi) b /= Nothing                    = False -- Check if the next coordinate has a piece
            | otherwise                                                  = isClear' (x2 + xi, y2 + yi) (xi, yi) -- Check next coordinate
