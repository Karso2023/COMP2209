--
-- PUT ANY IMPORT STATEMENTS HERE
import Data.Char (ord)
import Data.Ix (inRange)
--

--
-- DO NOT MODIFY THESE TYPES
--
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Show, Enum)

data Colour = White | Black
  deriving (Eq, Show)

data Square = Empty | Filled Colour Piece
  deriving (Eq, Show)

type Board = [[Square]]
type IndexedBoard = [(Square, (Int, Int))]

type Loc = (Int, Int)
------------------------------

-- For parsing string

-- Check if a location is within bounds
withinBounds :: Loc -> Bool
withinBounds (x, y) = all (inRange (0, 7)) [x, y]

-- Safe indexing to prevent negative index errors
safeIndex :: [[a]] -> Int -> Int -> Maybe a
safeIndex board y x
    | withinBounds (x, y) = Just ((board !! y) !! x)
    | otherwise = Nothing

-- Check if a location on the board is empty
isEmpty :: Board -> Loc -> Bool
isEmpty board (x, y) =
    case safeIndex board y x of
        Just Empty -> True
        _ -> False

-- Check if a location contains a piece of the same color
isSameColor :: Board -> Colour -> Loc -> Bool
isSameColor board color (x, y) =
    case safeIndex board y x of
        Just (Filled c _) -> c == color
        _ -> False

-- Get the piece at a location
getPieceAt :: Board -> Loc -> Maybe (Colour, Piece)
getPieceAt board (x, y) =
    case safeIndex board y x of
        Just (Filled color piece) -> Just (color, piece)
        _ -> Nothing

-- Pawn movement
pawnMoves :: Board -> Colour -> Loc -> [Loc]
pawnMoves board color (x, y) = filter withinBounds $
    case color of
        White ->
            let moves = [(x, y - 1) | isEmpty board (x, y - 1)] 
                doubleMove = [(x, y - 2) | y == 6, isEmpty board (x, y - 1), isEmpty board (x, y - 2)]
                captures = [(x - 1, y - 1), (x + 1, y - 1)] 
            in moves ++ doubleMove ++ filter (not . isEmpty board) captures

        Black ->
            let moves = [(x, y + 1) | isEmpty board (x, y + 1)] 
                doubleMove = [(x, y + 2) | y == 1, isEmpty board (x, y + 1), isEmpty board (x, y + 2)]
                captures = [(x - 1, y + 1), (x + 1, y + 1)] 
            in moves ++ doubleMove ++ filter (not . isEmpty board) captures

-- Rook movement
rookMoves :: Board -> Colour -> Loc -> [Loc]
rookMoves board color (x, y) =
    concatMap (filter withinBounds . lineMoves board color (x, y)) [(1, 0), (0, 1), (-1, 0), (0, -1)]

-- Bishop movement
bishopMoves :: Board -> Colour -> Loc -> [Loc]
bishopMoves board color (x, y) =
    concatMap (filter withinBounds . lineMoves board color (x, y)) [(1, 1), (1, -1), (-1, 1), (-1, -1)]

-- Queen movement (combination of rook and bishop moves)
queenMoves :: Board -> Colour -> Loc -> [Loc]
queenMoves board color (x, y) = rookMoves board color (x, y) ++ bishopMoves board color (x, y)

-- King movement (one square in any direction)
kingMoves :: Board -> Colour -> Loc -> [Loc]
kingMoves board color (x, y) =
    filter withinBounds $
    [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

-- Knight movement
knightMoves :: Board -> Colour -> Loc -> [Loc]
knightMoves board color (x, y) =
    filter withinBounds $
    [(x + dx, y + dy) | (dx, dy) <- [(2, 1), (2, -1), (-2, 1), (-2, -1), (1, 2), (1, -2), (-1, 2), (-1, -2)]]

-- Line moves for sliding pieces (rook, bishop, queen)
lineMoves :: Board -> Colour -> Loc -> (Int, Int) -> [Loc]
lineMoves board color (x, y) (dx, dy) =
    let nextPos (x, y) = (x + dx, y + dy)
        validMoves [] = []
        validMoves (loc:locs)
            | not (withinBounds loc) = []
            | isEmpty board loc = loc : validMoves locs
            | isSameColor board color loc = []
            | otherwise = [loc] -- Captures an opponent piece
    in validMoves (takeWhile withinBounds (iterate nextPos (x + dx, y + dy)))

-- Main function to get possible moves based on piece type
possMoves :: Board -> Loc -> [Loc]
possMoves board loc@(x, y) =
    case getPieceAt board loc of
        Nothing -> []
        Just (color, piece) ->
            case piece of
                Pawn   -> pawnMoves board color loc
                Rook   -> rookMoves board color loc
                Knight -> knightMoves board color loc
                Bishop -> bishopMoves board color loc
                Queen  -> queenMoves board color loc
                King   -> kingMoves board color loc
