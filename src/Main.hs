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

import Diagrams.TwoD.Puzzles.PuzzleDraw
import Diagrams.TwoD.Puzzles.Puzzle
import Data.Puzzles.Read (TypedPuzzle(..))

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Svg.Renderer.Text (renderSvg)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("puzzle", puzzlePostHandler) ] <|>
       dir "static" (serveDirectory "static")

fail400 :: String -> Snap ()
fail400 e = do
   modifyResponse $ setResponseStatus 400 "Bad Request"
   writeBS . C.pack $ "Bad request: " ++ e
   r <- getResponse
   finishWith r

serveDiagram :: SizeSpec2D -> Diagram B R2 -> Snap ()
serveDiagram sz d = do
    let svg = renderDia SVG (SVGOptions sz Nothing) d
    modifyResponse $ setContentType "image/svg+xml"
    writeLazyText $ renderSvg svg

cmtopix :: Double -> Double
cmtopix = (* 40)

dwidth :: Diagram B R2 -> Double
dwidth = fst . unr2 . boxExtents . boundingBox

sizeServeDiagram :: Diagram B R2 -> Snap ()
sizeServeDiagram d = serveDiagram (Width . cmtopix . dwidth $ d) d

decodeAndDrawPuzzle :: OutputChoice -> B.ByteString ->
                       Either String (Diagram B R2)
decodeAndDrawPuzzle oc b = decodeEither b >>= drawP
  where
    drawP :: TypedPuzzle -> Either String (Diagram B R2)
    drawP (TP t p ms) = parseEither (handleP oc t) (p, ms)
    handleP :: OutputChoice -> String -> (Value, Maybe Value) -> Parser (Diagram B R2)
    handleP DrawPuzzle   = handle drawPuzzle' fail
    handleP DrawSolution = handle drawSolution' fail
    handleP DrawExample  = handle drawExample' fail
    
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
    case decodeAndDrawPuzzle o (BL.toStrict body) of
        Left e   -> fail400 e
        Right d  -> sizeServeDiagram d
