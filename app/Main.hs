{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import ParseShak
import VimLike
import System.Environment
import System.IO
import qualified Data.ByteString.Char8 as B
import System.Console.ANSI
import Paths_sonnets


runChunks fn = runAction fn searchLine Nothing (printChunk, printList) . parseChunks


main :: IO ()
main = do 
          hSetBuffering stdin NoBuffering
          m <- getDataFileName "1041.txt"
          l <- B.readFile m
          runChunks "Sonnets" l
       
