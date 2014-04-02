{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class

import Snap.Core
import Snap.Util.FileServe
import Snap.Http.Server

import Diagrams.Prelude hiding (Result)
import Diagrams.BoundingBox
import Diagrams.Backend.SVG

import qualified Data.Aeson as J
import Data.Yaml

import Diagrams.TwoD.Puzzles.PuzzleDraw
import Diagrams.TwoD.Puzzles.Puzzle
import Data.Puzzles.Read (TypedPuzzle(..))

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Svg.Renderer.Text (renderSvg)

import System.Directory
import System.FilePath.Posix
import Data.List (stripPrefix)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site = route [ ("puzzle", puzzlePostHandler)
             , ("examples", examplesGetHandler) ] <|>
       dir "static" (serveDirectoryWith fancyDirectoryConfig "static")

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

data Example = Example { name :: String, path :: FilePath }

instance ToJSON Example where
    toJSON (Example n p) = object [ "name" .= n, "path" .= p ]

exampleFromPath :: FilePath -> Maybe Example
exampleFromPath fp = do
    guard $ takeExtension fp == ".pzl"
    n <- stripSuffix "-example" $ takeBaseName fp
    guard $ length n > 0
    return . Example n $ "/static" </> "examples" </> fp
  where
    stripSuffix s = fmap reverse . stripPrefix (reverse s) . reverse

listExamples :: IO [Example]
listExamples = do
    files <- getDirectoryContents "static/examples"
    return . catMaybes . map exampleFromPath $ files

examplesGetHandler :: Snap ()
examplesGetHandler = do
    examples <- liftIO listExamples
    writeLBS $ J.encode examples
