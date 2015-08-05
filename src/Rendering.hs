module Rendering where

import           Graphics.Gloss

import qualified Data.Matrix    as M
import qualified Data.Vector    as V

import           Game

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

                                  draw (c, r)   = renderCell (fromPos (r, c)) . texturePiece pics $ M.getElem r c board
                                  pieces        = map draw [(r, c) | r <- [1..8], c <- [1..8]]
                                  grid          = rectangleWire (8*pieceSize) (8*pieceSize)
                                  pieceSelected = renderSelected selected $  color (withAlpha 0.5 cyan) (rectangleSolid textureSize textureSize)
                                  pieceOver     = renderSelected over $ color red (rectangleSolid 56 56)
                                  selPicMoves   = (\pos -> renderCell (fromPos pos) (color blue $ circleSolid pieceSize) ) <$> selMoves
                                  overMoves     = maybe [] (movesAvailable board) over
                                  overPicMoves  = (\pos -> renderCell (fromPos pos) (color (withAlpha 0.5 blue) $ circleSolid pieceSize) ) <$> overMoves
                            in
                            Pictures (pieces ++ [grid, pieceSelected, pieceOver]++selPicMoves ++ overPicMoves)


renderCell :: (Float, Float) -> Picture -> Picture
renderCell (r, c) = translate (c*pieceSize - boardTranslation) (-r*pieceSize +boardTranslation) . scale (pieceSize / textureSize) (pieceSize / textureSize)
                    where boardTranslation = 4*pieceSize + pieceSize/2

texturePiece :: V.Vector Picture -> Maybe Piece -> Picture
texturePiece textures (Just (Pawn Black _))   = textures V.! 0
texturePiece textures (Just (Pawn White _))   = textures V.! 1
texturePiece textures (Just (Horse Black _))  = textures V.! 2
texturePiece textures (Just (Horse White _))  = textures V.! 3
texturePiece textures (Just (Bishop Black _)) = textures V.! 4
texturePiece textures (Just (Bishop White _)) = textures V.! 5
texturePiece textures (Just (Tower Black _))  = textures V.! 6
texturePiece textures (Just (Tower White _))  = textures V.! 7
texturePiece textures (Just (Queen Black _))  = textures V.! 8
texturePiece textures (Just (Queen White _))  = textures V.! 9
texturePiece textures (Just (King Black _))   = textures V.! 10
texturePiece textures (Just (King White _))   = textures V.! 11
texturePiece _ _                              = rectangleWire textureSize textureSize


renderSelected :: Maybe Piece -> Picture -> Picture
renderSelected piece selCursor = case piece of
                                   Just (Pawn _ (x, y))   -> render (fromPos (x, y))
                                   Just (Horse _ (x, y))  -> render (fromPos (x, y))
                                   Just (Tower _ (x, y))  -> render (fromPos (x, y))
                                   Just (Bishop _ (x, y)) -> render (fromPos (x, y))
                                   Just (Queen _ (x, y))  -> render (fromPos (x, y))
                                   Just (King _ (x, y))   -> render (fromPos (x, y))
                                   Nothing                -> Blank
                               where render (x,y) = renderCell (x, y) selCursor

