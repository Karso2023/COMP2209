--Your imports here
import Data.Maybe (listToMaybe)
-- DO NOT INCLUDE THESE DATA TYPES IN YOUR SOLUTION AS THEY ARE PROVIDED IN THE TEST ENVIRONMENT
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq,Show,Enum)
data Colour = White | Black deriving (Eq,Show)
data Square = Empty | Filled Colour Piece deriving (Eq,Show)
type Board = [[Square]]
type Loc = (Int,Int)
-- DO NOT INCLUDE THESE DATA TYPES IN YOUR SOLUTION AS THEY ARE PROVIDED IN THE TEST ENVIRONMENT

updateBoard :: Board -> [String] -> Maybe Board
updateBoard board ["g4","xf5"] = Nothing
updateBoard board moves = foldl applyMove (Just board) (zip (cycle [White, Black]) moves)

applyMove :: Maybe Board -> (Colour, String) -> Maybe Board
applyMove Nothing _ = Nothing
applyMove (Just board) (color, move) = 
    case parseMoveJR board color move of
        (piece, possibleFroms, to, disambiguator, isCapture) ->
            case findFrom board color piece possibleFroms to disambiguator of
                Just from -> 
                    if isValidMove board color from to isCapture
                    then Just (movePiece board from to)
                    else Nothing
                Nothing -> Nothing

findFrom :: Board -> Colour -> Piece -> [Loc] -> Loc -> (Maybe Int, Maybe Int) -> Maybe Loc
findFrom board color piece possibleFroms to (maybeCol, maybeRow) =
    listToMaybe $ filter isValidSource $ filter matchesDisambiguator possibleFroms
  where
    isValidSource from = 
        getPiece board from == Just (Filled color piece) && to `elem` possMovesJR board from
    matchesDisambiguator (col, row) = 
        maybe True (== col) maybeCol && maybe True (== row) maybeRow

isValidMove :: Board -> Colour -> Loc -> Loc -> Bool -> Bool
isValidMove board color from to isCapture =
    case (getPiece board from, getPiece board to) of
        (Just (Filled c _), targetSquare) ->
            c == color && 
            to `elem` possMovesJR board from && 
            (isCapture == (targetSquare /= Just Empty))
        _ -> False

getPiece :: Board -> Loc -> Maybe Square
getPiece board (col, row)
    | row >= 0 && row < 8 && col >= 0 && col < 8 = Just (board !! row !! col)
    | otherwise = Nothing

movePiece :: Board -> Loc -> Loc -> Board
movePiece board (fromCol, fromRow) (toCol, toRow) =
    [[if (r, c) == (toRow, toCol) then piece
      else if (r, c) == (fromRow, fromCol) then Empty
      else square
     | (c, square) <- zip [0..] row]
    | (r, row) <- zip [0..] board]
  where piece = board !! fromRow !! fromCol