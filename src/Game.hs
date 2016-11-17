module Game where

import           Control.Arrow ((***))
import           Control.Monad (join, replicateM)
import           Data.List     (foldl')
import qualified Data.Matrix   as M
import           Data.Maybe    (fromJust, isNothing, maybeToList)
import           Data.Monoid   (First (First), getFirst)

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
                            _playerMoving :: Player,
                            _gameEnded    :: Bool
                           } deriving Show

initialState :: GameState
initialState = GameState initialBoard Nothing [] Nothing White False








movesAvailable :: Board -> Piece -> [Pos]
movesAvailable board (Piece t o (r, c)) =
            let posOfElem (x,y) = maybe [] ((:[])._pos)  $ join $ M.safeGet x y board
                moves = case t of
                      Pawn
                          | r == 7 && o == White -> take 2 (allEmpty down) ++ posOfElem (r-1, c +1) ++ posOfElem (r-1,c-1)
                          | r == 2 && o == Black -> take 2 (allEmpty up)   ++ posOfElem (r+1, c +1) ++ posOfElem (r+1,c-1)
                          | o == White           -> take 1 (allEmpty down) ++ posOfElem (r-1, c +1) ++ posOfElem (r-1,c-1)
                          | o == Black           -> take 1 (allEmpty up)   ++ posOfElem (r+1, c +1) ++ posOfElem (r+1,c-1)
                          | otherwise            -> []

                      Horse  -> [(r+x, c+y) | [x, y] <- replicateM 2 [1, -1, 2, -2], abs x /= abs y]

                      Bishop -> untilFirst upleft ++ untilFirst upright ++ untilFirst downleft ++ untilFirst downright

                      Tower  -> untilFirst left ++ untilFirst right ++ untilFirst up ++ untilFirst down

                      Queen  -> foldMap untilFirst [left, right, up, down, upleft, upright, downleft, downright]

                      King   -> [(r+x, c+y) | x <- [-1..1], y <- [-1..1], r/=r+x || c/=c+y ]
            in
            filterFriendlyFire.filterOutside $  moves
            where
                filterOutside       = filter (\(x, y) -> x>0 && x <= 8 && y > 0 && y <= 8)
                filterFriendlyFire  = filter (\(x, y) -> maybe True (\piece -> _owner piece /= o)$M.getElem x y board)

                left   = [(r, c-y) | y <- [1..8]]
                right  = [(r, c+y) | y <- [1..8]]
                up     = [(r+y, c) | y <- [1..8]]
                down   = [(r-y, c) | y <- [1..8]]
                upleft    = [(r+y, c-y) | y <- [1..8]]
                downright = [(r-y, c+y) | y <- [1..8]]
                downleft  = [(r-y, c-y) | y <- [1..8]]
                upright   = [(r+y, c+y) | y <- [1..8]]

                firstPiece dir = getFirst $ foldMap First $ map (\(x,y) -> join $ M.safeGet x y board) dir
                allEmpty = takeWhile (\(x,y) -> isNothing $ join (M.safeGet x y board))
                untilFirst dir = allEmpty dir ++ maybeToList (_pos <$> firstPiece dir)







moveSelected :: GameState -> Pos -> GameState
moveSelected state newPs = let board = case fromJust $ _selected state of
                                           piece -> operation (piece {_pos = newPs}) (_pos piece)
                           in
                           state {_board = board, _selected = Nothing, _moves = [], _playerMoving = nextPlayer (_playerMoving state) }
                           where -- Move the piece from oldPos to newPs
                           operation piece p = M.setElem (Just piece) newPs (M.setElem Nothing p $ _board state)



getKings :: Board -> [Piece]
getKings = foldMap ( filter ((==King)._role).maybeToList)


findPiece :: Role -> Player -> Board -> [Piece]
findPiece role player = foldMap (filter condition.maybeToList)
                        where
                             condition = (&&) <$> (==role)._role <*> (==player)._owner

promotePawns :: Board -> Board
promotePawns board = foldl' (\b q -> M.setElem (Just q) (_pos q) b ) board queens
        where pawns      = foldMap ( filter condition.maybeToList) board
              condition  = (&&) <$> (==Pawn)._role <*> onEdge.fst._pos
              onEdge     = (||) <$> (==1) <*> (==8)
              queens     = map (\p -> p {_role = Queen}) pawns
