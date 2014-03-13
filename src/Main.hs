{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Diagrams.Prelude hiding (Result)
import Diagrams.BoundingBox
import Diagrams.Backend.SVG

import Data.Yaml
import Data.Aeson (Result(..))

import Diagrams.TwoD.Puzzles.PuzzleDraw
import Diagrams.TwoD.Puzzles.Puzzle
import Data.Puzzles.ReadPuzzle (TypedPuzzle)

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Text.Blaze.Svg.Renderer.Text (renderSvg)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("puzzle", puzzlePostHandler) ] <|>
       dir "static" (serveDirectory "static")

diagramHandler :: Int -> Snap ()
diagramHandler n = serveDiagram (Dims (fromIntegral n * 100) 100) (diagram n)

fail400 :: Snap ()
fail400 = do
   modifyResponse $ setResponseStatus 400 "Bad Request"
   writeBS "400 error"
   r <- getResponse
   finishWith r

diagramPostHandler :: Snap ()
diagramPostHandler = do
    body <- readRequestBody 4096
    case decode (toStrict body) of
        Just n  -> diagramHandler n
        Nothing -> fail400

diagram :: Int -> Diagram B R2
diagram n = hcat (replicate n (circle 1.0))

serveDiagram :: SizeSpec2D -> Diagram B R2 -> Snap ()
serveDiagram sz d = do
    let svg = renderDia SVG (SVGOptions sz Nothing) d
    modifyResponse $ setContentType "image/svg+xml"
    writeLazyText $ renderSvg svg

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

cmtopix :: Double -> Double
cmtopix = (* 40)

dwidth :: Diagram B R2 -> Double
dwidth = fst . unr2 . boxExtents . boundingBox

sizeServeDiagram :: Diagram B R2 -> Snap ()
sizeServeDiagram d = serveDiagram (Width . cmtopix . dwidth $ d) d

decodeAndDrawPuzzle :: ByteString -> Maybe (Diagram B R2, Diagram B R2)
decodeAndDrawPuzzle = maybe Nothing drawP . decode
  where
    drawP :: TypedPuzzle -> Maybe (Diagram B R2, Diagram B R2)
    drawP tp = case drawPuzzle tp of Error   _ -> Nothing
                                     Success d -> Just d
    

getOutputChoice :: Snap OutputChoice
getOutputChoice = do
    ocs <- maybe "puzzle" id <$> getParam "output"
    return $ case ocs of "solution" -> DrawSolution
                         "both"     -> DrawExample
                         _          -> DrawPuzzle

puzzlePostHandler :: Snap ()
puzzlePostHandler = do
    o <- getOutputChoice
    body <- readRequestBody 4096
    case decodeAndDrawPuzzle (toStrict body) of
        Just d  -> sizeServeDiagram (draw d o)
        Nothing -> fail400
