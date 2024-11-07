--
-- PUT ANY IMPORT STATEMENTS HERE
import Data.Char(ord)
--
-- Write a function

-- parseMove :: Board -> Colour -> String -> (Piece,[Loc],Loc,(Maybe Int,Maybe Int),Bool)

-- that, given a board state, a colour to say which player is to make the next move, and a string SAN move, returns a 5-tuple of information as follows:

-- Which type of piece is being moved
-- The source locations of all of the player's pieces of this type on the board
-- The target location of the move
-- A pair of possible locations representing extra disambiguating information used to identify the exact piece being moved.
-- A boolean value indicating whether an opponent's piece is taken in this move

-- BNF
-- <move> ::=  <location> |  'x' <location> | <piece> <location> | <piece> 'x' <location> 
-- <piece> ::= <pieceName> | <pieceName> <row> | <pieceName> <col> | <pieceName> <col> <row> 
-- <pieceName> ::= 'N' | 'B' | 'R' | 'Q' | 'K'
-- <col> ::= 'a' | ... | 'h'
-- <row> ::= 1 | ... | 8
-- <location> ::= <col> <row>

--
-- DO NOT MODIFY THESE TYPES
--
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq,Show,Enum)

data Colour = White | Black
  deriving (Eq,Show)

data Square = Empty | Filled Colour Piece
  deriving (Eq,Show)

type Board = [[Square]]
type IndexedBoard = [(Square,(Int,Int))]

type Loc = (Int,Int)
--------------------------------
-- For parsing string
pieceName :: Char -> Piece
pieceName 'N' = Knight
pieceName 'B' = Bishop
pieceName 'R' = Rook
pieceName 'Q' = Queen
pieceName 'K' = King
pieceName _ = error "Wrong input"

-- Convert SAN to int location
-- Indexing will range from 0 to 7 for both columns and rows with (0,0) being the top-left corner of the board
type SanLoc = (Char,Int)
sanLocConverter :: SanLoc -> Loc
sanLocConverter (col, row) = (ord col - ord 'a', 8 - row )

-- Parse the Move String: Check for presence of 'x' to determine if it’s a capture.
-- Extract the Piece and Target Location: Use pattern matching to handle different SAN cases.
-- Filter Pieces on the Board: Find all pieces of the specified type and color.
parseMove :: Board -> Colour -> String -> (Piece, [Loc], Loc, (Maybe Int, Maybe Int), Bool)
parseMove board colour string =
  let
      -- Determine if the move is a capture
      -- return True if it has 'x' in the string
      isCapture = 'x' `elem` string

      -- Remove capture character 'x'
      -- use "fxg6" as example, it became "fg6"
      cleanedString = filter (/= 'x') string

      -- Determine piece type
      -- (Pawn , fg6)
      (piece, rest) = if head cleanedString `elem` "NBRQK"
                      then (pieceName (head cleanedString), tail cleanedString)
                      else (Pawn, cleanedString)

      -- Target location
      -- (g,6)
      targetSanLoc = (last (init rest), read [last rest] :: Int)
      targetLoc = sanLocConverter targetSanLoc

      -- Disambiguation parsing
      disambiguation = case init rest of
          [col] | col `elem` ['a'..'h'] -> (Nothing, Nothing) 
          [row] | row `elem` ['1'..'8'] -> (Nothing, Nothing) 
          [col, row] | col `elem` ['a'..'h'] && row `elem` ['1'..'8'] -> 
                        (Nothing, Nothing) 
          _ -> (Nothing, Nothing)

      -- Find all locations of pieces of the specified type and colour
      allLocs = [(x, y) | (row, y) <- zip board [0..], (square, x) <- zip row [0..], 
            square == Filled colour piece]
  in
      (piece, allLocs, targetLoc, disambiguation, isCapture)



