module Rendering where

import           Graphics.Gloss

import qualified Data.Matrix    as M
import qualified Data.Vector    as V

import           Game


toColor :: Player -> Color
toColor White = white
toColor Black = black

-- Textures
texturesFiles :: [String]
texturesFiles = ["pawn", "horse", "bishop", "tower", "queen", "king"]
                >>= \ls -> [ls, ls ++ "_w"]
                >>= \ls' -> ["assets/" ++ ls' ++ ".png"]


pieceSize :: Float
pieceSize = 92

textureSize :: Float
textureSize = 512



-- | Convert our state to a picture.
renderFunction :: V.Vector Picture -> GameState -> Picture
renderFunction pics state = let   board         = _board state
                                  selected      = _selected state
                                  over          = _over state
                                  selMoves      = _moves state

                                  background    = [color (toColor (_playerMoving state)) $ rectangleSolid 1024 1024, color white $ rectangleSolid 800 800 ]
                                  draw (c, r)   = renderCell (fromPos (r, c)) . texturePiece pics $ M.getElem r c board
                                  pieces        = map draw [(r, c) | r <- [1..8], c <- [1..8]]
                                  grid          = rectangleWire (8*pieceSize) (8*pieceSize)
                                  pieceSelected = renderSelected selected $  color (withAlpha 0.5 cyan) (rectangleSolid textureSize textureSize)
                                  pieceOver     = renderSelected over $ color red (rectangleSolid 56 56)
                                  selPicMoves   = (\pos -> renderCell (fromPos pos) (color blue $ circleSolid pieceSize) ) <$> selMoves
                                  overMoves     = maybe [] (movesAvailable board) over
                                  overPicMoves  = (\pos -> renderCell (fromPos pos) (color (withAlpha 0.5 blue) $ circleSolid pieceSize) ) <$> overMoves
                                  gameOver      = translate (-365) 0 (if _gameEnded state then Text "Game Over" else Blank)
                            in
                            Pictures (background ++ pieces ++ [grid, pieceSelected, pieceOver]++selPicMoves ++ overPicMoves ++ [gameOver])


renderCell :: (Float, Float) -> Picture -> Picture
renderCell (r, c) = translate (c*pieceSize - boardTranslation) (-r*pieceSize +boardTranslation) . scale cellScale cellScale

                    where boardTranslation = 4*pieceSize + pieceSize/2
                          cellScale = pieceSize / textureSize

texturePiece :: V.Vector Picture -> Maybe Piece -> Picture
texturePiece textures (Just (Piece Pawn Black _))   = textures V.! 0
texturePiece textures (Just (Piece Pawn White _))   = textures V.! 1
texturePiece textures (Just (Piece Horse Black _))  = textures V.! 2
texturePiece textures (Just (Piece Horse White _))  = textures V.! 3
texturePiece textures (Just (Piece Bishop Black _)) = textures V.! 4
texturePiece textures (Just (Piece Bishop White _)) = textures V.! 5
texturePiece textures (Just (Piece Tower Black _))  = textures V.! 6
texturePiece textures (Just (Piece Tower White _))  = textures V.! 7
texturePiece textures (Just (Piece Queen Black _))  = textures V.! 8
texturePiece textures (Just (Piece Queen White _))  = textures V.! 9
texturePiece textures (Just (Piece King Black _))   = textures V.! 10
texturePiece textures (Just (Piece King White _))   = textures V.! 11
texturePiece _ _                              = rectangleWire textureSize textureSize


renderSelected :: Maybe Piece -> Picture -> Picture
renderSelected piece selCursor = maybe Blank (render.fromPos._pos ) piece
                                 where render (x,y) = renderCell (x, y) selCursor
