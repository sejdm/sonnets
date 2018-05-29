{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module ParseShak
    ( 
      parseChunks
    , printChunk
    , searchLine
    ) where

import Data.List
import Data.Char
import Data.List.Split
import Control.Monad.State
import Control.Applicative
import Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Rainbow as R





parseChunks :: B.ByteString -> [B.ByteString]
parseChunks = take 154 . map B.unlines . filter ((>6) . length) . splitOn [""] . dropWhile (/="  I") . B.lines

printChunk :: (Int, B.ByteString) -> IO ()
printChunk (n, c) = do
  putStr $ (take 4 (show n ++ ('.' : repeat ' ')))
  R.putChunkLn $ R.chunk (B.unpack (head $ B.lines c)) & R.italic
  putStrLn ""
  B.putStr c


searchLine x y = B.map toLower x `B.isInfixOf` B.map toLower y 

