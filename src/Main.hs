module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import qualified Data.Matrix                        as M
import qualified Data.Vector                        as V

import           Data.Maybe                         (fromJust)
import           Graphics.Gloss.Juicy               (loadJuicyPNG)

import           Game
import           Rendering

--Debug  trace
{-import           Debug.Trace-}




main :: IO ()
main = do
    assets <- sequence $ fmap  loadJuicyPNG texturesFiles
    play (InWindow "Draw" (1024, 1024) (0,0))
        white 100 initialState
        (renderFunction $ vectorFrom assets) handleEvent updateFunction
    where
        vectorFrom = V.fromList . fromJust . sequence



-- | Handle mouse click and motion events.
handleEvent :: Event -> GameState -> GameState
handleEvent event state
        -- If the mouse has moved, then extend the current line.
        | EventMotion (x,y)  <- event
        = state {_selected = selectPiece (round x, round y) (_board state)}

        {--- Start drawing a new line.-}
        {-| EventKey (MouseButton LeftButton) Down _  _ <- event-}
        {-= state & stateColor .~ blue-}

        {--- Finish drawing a line, and add it to the picture.-}
        {-| EventKey (MouseButton LeftButton) Up _ _       <- event-}
        {-= state & stateColor .~ black-}

        {--- Move-}
        {-| EventKey (SpecialKey key) Up _ _ <- event-}
        {-=-}
           {-case key of-}
                    {-KeyLeft  -> state & stateTranslation._1 -~ 80-}
                    {-KeyRight -> state & stateTranslation._1 +~ 80-}
                    {-KeyUp    -> state & stateTranslation._2 +~ 80-}
                    {-KeyDown  -> state & stateTranslation._2 -~ 80-}
                    {-_ -> state-}


        | otherwise
        = state


selectPiece :: Pos -> Board -> Maybe Piece
selectPiece (x,y) = if row < 1 || row >8 || col < 1 || col > 8 then const Nothing else  M.getElem row col
                    where
                        row = -(div y size - 3) + 1
                        col = div x size + 5
                        size = round pieceSize



updateFunction :: Float -> GameState -> GameState
updateFunction _ = id

