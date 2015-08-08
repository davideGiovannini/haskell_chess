module Game where

import           Control.Arrow ((***))
import           Control.Monad (replicateM)
import qualified Data.Matrix   as M
import           Data.Maybe    (fromJust)

type Pos = (Int, Int)

fromPos :: Pos -> (Float, Float)
fromPos = fromIntegral *** fromIntegral


data Player = White
            | Black
            deriving (Show, Eq)

nextPlayer :: Player -> Player
nextPlayer White = Black
nextPlayer Black = White


data Role = Pawn
           | Horse
           | Bishop
           | Tower
           | Queen
           | King
           deriving (Show, Eq)

data Piece = Piece {
                    _role  :: Role,
                    _owner :: Player,
                    _pos   :: Pos
                   } deriving (Show, Eq)



type Board = M.Matrix (Maybe Piece)

initialBoard :: Board
initialBoard = M.matrix 8 8 _initialBoardGenerator


_initialBoardGenerator :: (Int, Int) -> Maybe Piece
_initialBoardGenerator pos@(row, column)
                        | row == 2 || row == 7                                   = Just (Piece Pawn (playerFrom row) pos)
                        | (column == 1 || column == 8) && (firstRow || lastRow)  = Just (Piece Tower (playerFrom row) pos)
                        | (column == 2 || column == 7) && (firstRow || lastRow)  = Just (Piece Horse (playerFrom row) pos)
                        | (column == 3 || column == 6) && (firstRow || lastRow)  = Just (Piece Bishop (playerFrom row) pos)
                        | column == 4 && lastRow                                 = Just (Piece Queen White pos)
                        | column == 5 && firstRow                                = Just (Piece Queen Black pos)
                        | column == 5 && lastRow                                 = Just (Piece King White pos)
                        | column == 4 && firstRow                                = Just (Piece King Black pos)
                        | otherwise                                              = Nothing

                        where playerFrom n = if n < 3 then Black else White
                              firstRow = row == 1
                              lastRow  = row == 8


data GameState = GameState {
                            _board        :: Board,
                            _selected     :: Maybe Piece,
                            _moves        :: [Pos],
                            _over         :: Maybe Piece,
                            _playerMoving :: Player
                           } deriving Show

initialState :: GameState
initialState = GameState initialBoard Nothing [] Nothing White








movesAvailable :: Board -> Piece -> [Pos]
movesAvailable board (Piece t o (r, c)) = let moves = case t of
                                                    Pawn
                                                        | r == 7 && o == White  -> [(r-1, c), (r-2, c)]
                                                        | r == 2 && o == Black  -> [(r+1, c), (r+2, c)]
                                                        | o == White            -> [(r-1, c)]
                                                        | o == Black            -> [(r+1, c)]
                                                        | otherwise             -> []

                                                    Horse  -> [(r+x, c+y) | [x, y] <- replicateM 2 [1, -1, 2, -2], abs x /= abs y]

                                                    Bishop -> [(r+x, c+y) | x <- [-8..8], y <- [-8..8], bishopCondition x y]

                                                    Tower  -> [(r+x, c+y) | x <- [-8..7], y <- [-8..7],  towerCondition x y]

                                                    Queen  -> [(r+x, c+y) | x <- [-8..8], y <- [-8..8], bishopCondition x y || towerCondition x y]

                                                    King   -> [(r+x, c+y) | x <- [-1..1], y <- [-1..1], r/=r+x || c/=c+y ]
                                         in
                                         filterFriendlyFire.filterOutside $  moves
                                         where
                                             filterOutside       = filter (\(x, y) -> x>0 && x <= 8 && y > 0 && y <= 8)
                                             filterFriendlyFire  = filter (\(x, y) -> maybe True (\piece -> _owner piece /= o)$M.getElem x y board)
                                             bishopCondition x y = abs x == abs y && x /= 0
                                             towerCondition  x y = (x == 0 || y == 0) && x /=y




moveSelected :: GameState -> Pos -> GameState
moveSelected state newPs = let board = case fromJust $ _selected state of
                                           piece -> operation (piece {_pos = newPs}) (_pos piece)
                           in
                           state {_board = board, _selected = Nothing, _moves = [], _playerMoving = nextPlayer (_playerMoving state) }
                           where -- Move the piece from oldPos to newPs
                           operation piece p = M.setElem (Just piece) newPs (M.setElem Nothing p $ _board state)



