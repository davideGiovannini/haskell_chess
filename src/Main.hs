module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import qualified Data.Matrix                        as M
import qualified Data.Vector                        as V

import           Data.Maybe                         (fromJust, isJust)
import           ImageLoading                       (loadJuicyPNG)

import           Control.Monad                      (mfilter)
import           Game
import           Rendering

--Debug  trace
{-import           Debug.Trace-}




main :: IO ()
main = do
    assets <- sequence (loadJuicyPNG <$> texturesFiles)
    play (InWindow "Draw" (1024, 1024) (0,0))
        white 100 initialState
        (renderFunction $ vectorFrom assets) handleEvent updateFunction
    where
        vectorFrom = V.fromList . fromJust . sequence



-- | Handle mouse click and motion events.
handleEvent :: Event -> GameState -> GameState
handleEvent event state
        -- If the mouse has moved, then extend the current line.
        | EventMotion screenPos  <- event
        = let pos = fromScreenToBoard screenPos
          in
          state {_over = selectPiece pos (_board state)}


        | EventKey (MouseButton LeftButton) Down _  screenPos <- event, not (_gameEnded state)
        = let pos          = fromScreenToBoard screenPos
              selected     = selectPiece pos (_board state)
              playerMoving = _playerMoving state
              maybeMoving  = isJust (_selected state)
          in
          if maybeMoving && pos `elem` _moves state
          then -- Check if I'm trying to mov
            moveSelected state pos
          else -- Select A piece
              let sel = mfilter (\piece -> _owner piece == playerMoving) selected
              in
              state {
                     _selected = sel,
                     _moves    = maybe [] (movesAvailable (_board state)) sel
                    }


        | EventKey (SpecialKey KeySpace) Down _ _ <- event, _gameEnded state
        = state {_board= initialBoard, _gameEnded = False}


        | otherwise
        = state


selectPiece :: Pos -> Board -> Maybe Piece
selectPiece (row,col) = if row < 1 || row >8 || col < 1 || col > 8 then const Nothing else  M.getElem row col


fromScreenToBoard :: (Float, Float) -> Pos
fromScreenToBoard (x, y) = (row, col)
                    where
                        (x', y') = (round x, round y)
                        row      = -(div y' size - 3) + 1
                        col      = div x' size + 5
                        size     = round pieceSize


updateFunction :: Float -> GameState -> GameState
updateFunction _ state = if length (getKings board) < 2
                         then state {_gameEnded = True}
                         else state {_board = promotePawns board}
                         where board = _board state
