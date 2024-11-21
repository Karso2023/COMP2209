--Your imports here

-- DO NOT INCLUDE THESE DATA TYPES IN YOUR SOLUTION AS THEY ARE PROVIDED IN THE TEST ENVIRONMENT
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq,Show,Enum)
data Colour = White | Black deriving (Eq,Show)
data Square = Empty | Filled Colour Piece deriving (Eq,Show)
type Board = [[Square]]
type Loc = (Int,Int)
-- DO NOT INCLUDE THESE DATA TYPES IN YOUR SOLUTION AS THEY ARE PROVIDED IN THE TEST ENVIRONMENT

updateBoard :: Board -> [String] -> Maybe Board
updateBoard = undefined
