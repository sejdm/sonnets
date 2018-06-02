{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction, TemplateHaskell #-}
module VimLike
  (
    runVimList
  , keepApp
  , showBoth
  , showSearch
  , showSurroundList
  , showCurrent
  , showUsingLine
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
import Control.Monad
import Control.Lens
import Control.Applicative

-- Helper functions
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


-- Recursive actions

data RecursiveAction r = RecursiveAction (IO ()) (r -> Maybe (RecursiveAction r))
-- The type variable r maybe Char or String
-- depending on whether it is reacting to a keystroke or an entered string

usingString = (getLine, "quit")
usingChar = (getChar, 'q')


runAction (u, q) h (RecursiveAction i f) = do
  system "clear" >> h >> i
  c <- u
  case f c of
    Nothing ->
          if (c == q) then showCursor >> return ()
          else i
    Just z -> runAction (u, q) h z



showTitle t = hideCursor >> boldShow t >> putStrLn ""
  where boldShow t = putChunkLn $ chunk ("     " ++ map toUpper t) & bold

getFileName fn = takeWhile (/='.') $ last $ splitOn "/" fn

showFile = showTitle . getFileName



-- Modes
type Mode' c a = c -> a -> Maybe a
type Mode a = Mode' Char a

modeToAction :: (a -> IO ()) -> Mode' c a -> a -> RecursiveAction c
modeToAction p f a = RecursiveAction (p a) $ \c ->
  modeToAction p f <$> f c a
  

keepApp :: FilePath -> (a -> IO ()) -> Mode a -> a -> IO ()
keepApp fn p f = runAction usingChar (showFile fn) . modeToAction p f

  
-- VimState
data VimState a = VimState {
    _number :: (Maybe Int)
  , _buffer :: (M.Map Char Int)
  , _currentMode :: (Mode (VimState a))
  , _getValue :: a
  , _searchTerm :: B.ByteString
  , _lastJumped :: Int
  }

makeLenses ''VimState

getCurrent = head . snd . _getValue

-- Main exported function
runVimList fn se f xs = keepApp fn f vimStatemode $
  VimState {
    _number = Nothing
  , _buffer = M.empty
  , _searchTerm = ""
  , _currentMode = defaultVim se
  , _getValue = ([], zip [1..] xs)
  , _lastJumped = 1
  }



-- Printing convenience functions
showSurroundList (fl1, fl2) (xs, (y:ys)) = mapM_ f (take 20 (zip (repeat False) (reverse (take 10 xs)) ++ ((True, y) :zip (repeat False) ys)))
  where f (True, x) = fl1 x
        f (False, x) = fl2 x

showSearch x = (putChunk $ chunk ("Search Term: " :: B.ByteString) & italic) >> case _searchTerm x of
  "" -> B.putStrLn ""
  s -> B.putStr s

showBoth (f, fl1, fl2) x = f (getCurrent x) >> replicateM_ 3 (putStrLn "") >> showSurroundList (fl1, fl2) (_getValue x) >> putStrLn "" >> showSearch x 

showCurrent f = f . getCurrent


viewChunkList l (n, b) = B.pack (take 4 (show n ++ ('.' : repeat ' '))) `B.append` l b

printListHighlighted l c = (putChunkLn $ chunk (B.unpack $ viewChunkList l c) & fore red)
printListUnHighlighted l c = B.putStrLn $ viewChunkList l c

showUsingLine (f, l) = showBoth (f, printListHighlighted l, printListUnHighlighted l)


-- Multiple applications

multipleApp f v = v & number .~ Nothing
  & getValue .~ case v ^. number of
                  Nothing -> f (v ^. getValue)
                  Just n ->  applyMany n f (v ^. getValue)

vimStatemode c a = view currentMode a c a

-- Mode convenience functions

multi' f =  Just . multipleApp f
onlyOn' o f =  Just . over o f
assignTo' s x =  Just . set s x


multi f a = setLastJumped a $ multipleApp f a
onlyOn o f a = setLastJumped a $ over o f a
assignTo s x a = setLastJumped a $ set s x a

noJump = const Just
jump = setLastJumped

setLastJumped v x = Just $ x & lastJumped .~ fst (getCurrent v)

thenTry m1 m2 = \c a -> m1 c a <|> m2 c a


-- Various convenient modes

searchMode se sf mo' c = 
  case c of
    '\n' -> (\x -> onlyOn' getValue (sf se (x ^. searchTerm)) x) >=> assignTo' currentMode mo'
    '\DEL' -> onlyOn' searchTerm (B.reverse . tail' . B.reverse )
    '\ESC' -> assignTo' searchTerm "" >=> assignTo' currentMode mo'
    c' -> onlyOn' searchTerm (`B.append` B.pack [c'])
  where tail' "" = ""
        tail' x = B.tail x

mapMode mo' c v = Just $
  v { _buffer = (case (snd (_getValue v)) of
                ((i,_):_) -> M.insert c i (_buffer v)
                _ -> (_buffer v))
    , _currentMode = mo' }


mapRetrieveMode mo' x v = Just $
  (_buffer v) `seq` v {_getValue =  (case (snd (_getValue v)) of
                                    ((n',_):_) -> move (M.findWithDefault n' x (_buffer v) - n') (_getValue v)
                                    _ -> _getValue v), _currentMode = mo'}



defaultVim se = normalMode se `thenTry` digitMode

normalMode se c = case c of
  'j' -> multi next
  'k' -> multi previous
  'm' -> onlyOn currentMode mapMode
  '`' -> onlyOn currentMode mapRetrieveMode
  '/' -> onlyOn currentMode (searchMode se search) >=> assignTo searchTerm ""
  '?' -> onlyOn currentMode (searchMode se searchBackward) >=> assignTo searchTerm ""
  'n' -> onlyOn getValue next >=> \x -> multi (search se (x ^. searchTerm)) x
  'p' -> onlyOn getValue next >=> \x -> multi (searchBackward se (x ^. searchTerm)) x
  'G' -> onlyOn getValue end

  'g' -> \a -> case a ^. number of
               Nothing -> onlyOn getValue beginning a
               _ -> (multi next <=< onlyOn number ((\x -> x - 1) <$>)
                    <=< onlyOn getValue beginning) a

  'o' -> (\a -> assignTo number (Just (a ^. lastJumped - 1)) a) >=> onlyOn getValue beginning >=> multi next

  _ -> const Nothing

digitMode c x | isDigit c =  case (x ^. number) of
                               Nothing -> assignTo number (Just n') x
                               Just n'' -> n'' `seq` assignTo number (Just (10*n'' + n')) x
              | otherwise = Nothing 
              where n' = read [c]
