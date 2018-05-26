{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module VimLike
  (
    runAction
  ) where

import Rainbow
import System.Process
import Data.Char
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B
import Data.List.Split (splitOn)
import System.Console.ANSI
import Control.DeepSeq
import Control.Seq

applyMany 0 f x = x
applyMany n f x | n >= 0 = (x `using` seqTuple2 (seqListN 1 rdeepseq) (seqListN 1 rdeepseq)) `seq` applyMany (n-1) f (f x)
                | otherwise = x


move n | n >= 0 = applyMany n next
       | otherwise = applyMany (abs n) previous


next a@(_, []) = a
next (xs, ys@[_]) = (xs, ys)
next (xs, (y:ys)) = ((y:xs), ys)


previous (xs@[], ys) = (xs, ys)
previous ((x:xs), ys) = (xs, (x:ys))


beginning a@([], _) = a
beginning (x:xs, ys) = beginning (xs, x:ys)

end a@(_, []) = a
end a@(_, [y]) = a
end a@(xs, y:ys) = end (y:xs, ys)

search se s a@(xs, ys) = case span (not . se s . snd) ys of
  (_, []) -> a
  (ds, rs) -> (reverse ds ++ xs, rs)


searchBackward se s a@(xs, ys) = case span (not . se s . snd) xs of
  (_, []) -> a
  (ds, r:rs) -> (rs , r:reverse ds ++ ys)


runAction fn se n f xs = runAct fn (se, Nothing, n, M.empty) f ([], zip [1..] xs)

viewChunkList (n, b) = B.pack (show n) `B.append` ". " `B.append` head (B.lines b)

printChnk (True, c) = (putChunkLn $ chunk (B.unpack $ viewChunkList c) & fore red)
printChnk (_, c) = B.putStrLn $ viewChunkList c

runAct fn (se, st, n,m) (f, fl) a@(xs, ys) =
  do system "clear"
     hideCursor
     putChunkLn $ chunk ("     " ++ (map toUpper $ takeWhile (/='.') $ last $ splitOn "/" fn)) & bold
     putStrLn ""
     case ys of
       (y:ys) -> f y >> putStrLn ""  >> putStrLn ""  >> putStrLn "" 
         -- >> mapM_ B.putStrLn (reverse $ map viewChunkList $ take 3 xs)
         -- >> (putChunkLn $ chunk (B.unpack $ viewChunkList y) & fore red)
         -- >> mapM_ B.putStrLn (map viewChunkList $ take 6 ys)
         >> mapM_ fl (take 20 (zip (repeat False) (reverse (take 10 xs)) ++ ((True, y) :zip (repeat False) ys)))
       _ -> return ()
     c <- getChar

     

     case c of
         'j' ->  runAct fn (se,st, Nothing, m) (f,fl) (applyMany (buff n) next a)

         'k' ->  runAct fn (se,st, Nothing, m) (f,fl) (applyMany (buff n) previous a)

         'm' -> getChar >>= \x -> m `seq` runAct fn (se,st, n, (case ys of ((i,_):_) -> M.insert x i m; _ -> m)) (f,fl) a

         '`' -> getChar >>= \x -> m `seq` runAct fn (se,st, Nothing, m) (f,fl) (case ys of ((n',_):_) -> move (M.findWithDefault n' x m - n') a; _ -> a)

         'g' -> runAct fn (se,st, Nothing, m) (f,fl) (applyMany (buff n - 1) next (beginning a))

         'G' -> runAct fn (se,st, n, m) (f,fl) (end a)

         '/' -> B.getLine >>= \s' -> runAct fn (se,Just s', n, m) (f,fl) (search se s' a)

         '?' -> B.getLine >>= \s' -> runAct fn (se,Just s', n, m) (f,fl) (searchBackward se s' a)

         'n' -> runAct fn (se,st, Nothing, m) (f,fl) (case st of Just s' -> applyMany (buff n) (search se s' . next) a; _ -> a)

         'p' -> runAct fn (se,st, Nothing, m) (f,fl) (case st of Just s' -> applyMany (buff n) (searchBackward se s' . previous) a; _ -> a)

         'q' -> showCursor >> return ()

         x -> runAct fn (se,st, (case n of Nothing -> Just (read [x]); Just n' -> Just (10*n' + read [x])), m) (f,fl) a

  where buff n = case n of Nothing -> 1; Just n' -> n'




