module Game where

import           Control.Arrow ((***))
import           Control.Monad (replicateM)
import qualified Data.Matrix   as M
import           Data.Maybe    (isNothing)

type Pos = (Int, Int)

fromPos :: Pos -> (Float, Float)
fromPos = fromIntegral *** fromIntegral


data Player = White
           | Black
           deriving (Show, Eq)

data Piece = Pawn   Player Pos
           | Horse  Player Pos
           | Bishop Player Pos
           | Tower  Player Pos
           | Queen  Player Pos
           | King   Player Pos
           deriving (Show, Eq)


type Board = M.Matrix (Maybe Piece)

initialBoard :: Board
initialBoard = M.matrix 8 8 _initialBoardGenerator


_initialBoardGenerator :: (Int, Int) -> Maybe Piece
_initialBoardGenerator pos@(row, column)
                        | row == 2 || row == 7 = Just (Pawn (playerFrom row) pos)
                        | (column == 1 || column == 8) && (firstRow || lastRow)  = Just (Tower (playerFrom row) pos)
                        | (column == 2 || column == 7) && (firstRow || lastRow)  = Just (Horse (playerFrom row) pos)
                        | (column == 3 || column == 6) && (firstRow || lastRow)  = Just (Bishop (playerFrom row) pos)
                        | column == 4 && lastRow                                 = Just (Queen White pos)
                        | column == 5 && firstRow                                = Just (Queen Black pos)
                        | column == 5 && lastRow                                 = Just (King White pos)
                        | column == 4 && firstRow                                = Just (King Black pos)


                        | otherwise                                              = Nothing

                        where playerFrom n = if n < 3 then Black else White
                              firstRow = row == 1
                              lastRow  = row == 8


data GameState = GameState {
                            _board    :: Board,
                            _selected :: Maybe Piece
                           } deriving Show

initialState :: GameState
initialState = GameState initialBoard Nothing








movesAvailable :: Board -> Piece -> [Pos]
movesAvailable board piece = let moves = case piece of
                                            Pawn White (r, c)
                                                    | r == 7      -> [(r-1, c), (r-2, c)]
                                                    | otherwise   -> [(r-1, c)]
                                            Pawn Black (r, c)
                                                    | r == 2      -> [(r+1, c), (r+2, c)]
                                                    | otherwise   -> [(r+1, c)]

                                            Horse _ (r, c)        -> [(r+x, c+y) | [x, y] <- replicateM 2 [1, -1, 2, -2], abs x /= abs y]

                                            Bishop _ (r, c)       -> [(r+x, c+y) | x <- [-8..8], y <- [-8..8], bishopCondition x y]

                                            Tower _ (r, c)        -> [(r+x, c+y) | x <- [-8..7], y <- [-8..7],  towerCondition x y]

                                            Queen _ (r, c)        -> [(r+x, c+y) | x <- [-8..8], y <- [-8..8], bishopCondition x y || towerCondition x y]

                                            King _ (r, c)        -> [(r+x, c+y) | x <- [-1..1], y <- [-1..1], r/=r+x || c/=c+y ]
                             in
                             {-filterOccupied . filterOutside $ moves-}
                             filterOutside $ moves
                             where
                                 filterOutside   = filter (\(x, y) -> x>0 && x <= 8 && y > 0 && y <= 8)
                                 filterOccupied  = filter (\(x, y) -> isNothing $ M.getElem x y board)
                                 bishopCondition x y = abs x == abs y && x /= 0
                                 towerCondition  x y = (x == 0 || y == 0) && x /=y

