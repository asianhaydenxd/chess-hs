module Game where

import State

data Capture = NoCapture | Capture | EnPassant

data Promotion = NoPromotion | Promotion Piece

data Move = Move Coord Coord Capture Promotion | WhiteLongCastle | WhiteShortCastle | BlackLongCastle | BlackShortCastle

type Game = [Move]
