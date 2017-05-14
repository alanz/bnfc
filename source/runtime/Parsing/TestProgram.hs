{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Parsing.TestProgram where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import GHC.Exts
import Control.Monad
import Control.Applicative (pure)
import Parsing.Chart hiding (fingerprint,mkTree)
import Data.Matrix.Quad
import Data.Pair
import Algebra.RingUtils
import Data.FingerTree

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()


mainTest :: forall category token.
            (RingP [(category,Any)], Eq category)
         => ((category,Any) -> String)                -- ^ Show function
         -> (Bool -> token -> Pair [(category,Any)])  -- ^ convert a token to a category
         -> (String -> [token])                       -- ^ lex a string into tokens
         -> (token -> (Int,Int))                      -- ^ extract position from a token
         -> (category -> String)                      -- ^ describe a category
         -> (category -> [category])                  -- ^ given following categories for a given one
         -> IO ()
mainTest showAst cnfToksToCat myLLexer getTokPos describe follows =
  do args <- getArgs
     case args of
       [] -> hGetContents stdin >>= run "stdin" 2
       "-s":fs -> mapM_ (runFile 0) fs
       fs -> mapM_ (runFile 2) fs

 where
  neighbors a b = b `elem` follows a
  showResults :: [(category,Any)] -> IO ()
  showResults x = do
        putStrLn $ show (length x) ++ " results"
        forM_ x $ \(cat,ast) -> do
          putStrLn $ describe cat
          putStrLn $ showAst (cat,ast)

  runFile v f = putStrLn f >> readFile f >>= run f v
  run f v s =
    do case rs of
         [(_,x,_)] -> showResults x
         _ -> do let errs = pairs rs
                     best = minimum $ map quality errs
                 mapM_ (putStrLn . showErr ts) $ filter (\x -> quality x == best) errs
       when (v >= 2) $ do
         writeFile (f ++ ".xpm") (genXPM $ fingerprint chart)
         let scatt = scatterplot chart
         putStrLn $ "Scatterplot data size:" ++ show (length scatt)
         writeFile (f ++ ".data") scatt
    where
          ts :: [token]
          ts = myLLexer s
          chart :: SomeTri [(category, Any)]
          chart = mkTree $ zipWith cnfToksToCat (cycle [False,True]) ts
          rs :: [(Int, [(category, Any)], Int)]
          rs = results chart

  showTokPos :: (Int,Int) -> String
  showTokPos (l,c) = show l ++ "," ++ show (c-1)

  showPos :: [token] -> Int -> String
  showPos ts x = showTokPos (getTokPos $ ts !! x)

  showErr ts ((_,x',p),(_,y',_)) =
     showPos ts p ++ ": cannot combine " ++ showBestCat x' ++ " with " ++ showBestCat y'

  quality (a@(_,x',p),b@(_,y',_)) = (or [ neighbors x y | x <- map fst x', y <- map fst y'],
                                     (resSz a) Prelude.+ (resSz b))


  showBestCat ((x,_):_) = describe x

pairs (x:y:xs) = (x,y):pairs (y:xs)
pairs _ = []

resSz (i,_,j) = j-i


mainTestIncremental :: forall category token inttoken.
            (RingP [(category,Any)], Eq category, Show category)
         => ((category,Any) -> String)                -- ^ Show function
         -> (Bool -> token -> Pair [(category,Any)])  -- ^ convert a token to a category
         -- -> (String -> [(category,Any)])              -- ^ lex a string into tokens
         -> (String -> FingerTree (SomeTri [(category,Any)]) inttoken)-- ^ lex a string into tokens
         -> (FingerTree (SomeTri [(category,Any)]) inttoken -> SomeTri [(category, Any)])
         -> (token -> (Int,Int))                      -- ^ extract position from a token
         -> (category -> String)                      -- ^ describe a category
         -> (category -> [category])                  -- ^ given following categories for a given one
         -> IO ()
mainTestIncremental showAst cnfToksToCat myLLexer myParser getTokPos describe follows =
  do args <- getArgs
     case args of
       [] -> hGetContents stdin >>= run "stdin" 2
       "-s":fs -> mapM_ (runFile 0) fs
       fs -> mapM_ (runFile 2) fs

 where
  neighbors a b = b `elem` follows a
  showResults :: [(category,Any)] -> IO ()
  showResults x = do
        putStrLn $ show (length x) ++ " results"
        forM_ x $ \(cat,ast) -> do
          putStrLn $ describe cat
          putStrLn $ showAst (cat,ast)

  runFile v f = putStrLn f >> readFile f >>= run f v
  run f v s =
    do case rs of
         [(_,x,_)] -> showResults x
         _ -> do let errs = pairs rs
                     best = minimum $ map quality errs
                 mapM_ (putStrLn . showErr ts) $ filter (\x -> quality x == best) errs
       when (v >= 2) $ do
         writeFile (f ++ ".xpm") (genXPM $ fingerprint chart)
         let scatt = scatterplot chart
         putStrLn $ "Scatterplot data size:" ++ show (length scatt)
         writeFile (f ++ ".data") scatt
    where
          ts :: FingerTree (SomeTri [(category,Any)]) inttoken
          ts = myLLexer s
          -- chart = mkMyTree [mkMyPair ts]
          -- chart = mkTree $ zipWith cnfToksToCat (cycle [False,True]) ts
          chart :: SomeTri [(category, Any)]
          chart = myParser ts
          -- chart = measure ts
          rs :: [(Int, [(category, Any)], Int)]
          rs = results chart
          -- rs = parse ts

  showTokPos :: (Int,Int) -> String
  showTokPos (l,c) = show l ++ "," ++ show (c-1)

  showPos :: [token] -> Int -> String
  showPos ts x = showTokPos (getTokPos $ ts !! x)

  -- showErr ts ((_,x',p),(_,y',_)) =
  --    showPos ts p ++ ": cannot combine " ++ showBestCat x' ++ " with " ++ showBestCat y'
  -- showErr ts ((_,x',p),(_,y',_)) =
  --    show (map fst ts) ++ ": cannot combine " ++ showBestCat x' ++ " with " ++ showBestCat y'
  showErr ts ((_,x',p),(_,y',_)) =
      ": cannot combine " ++ showBestCat x' ++ " with " ++ showBestCat y'

  quality (a@(_,x',p),b@(_,y',_)) = (or [ neighbors x y | x <- map fst x', y <- map fst y'],
                                     (resSz a) Prelude.+ (resSz b))


  showBestCat ((x,_):_) = describe x

  mkMyPair :: [(category,Any)] -> Pair [(category,Any)]
  mkMyPair ts = f :/: b
    where
      (f,b) = splitAt (length ts `div` 2) ts

  mkMyTree :: [Pair [(category,Any)] ] -> SomeTri [(category,Any)]
  mkMyTree = mkTree

  myResults :: SomeTri [(category,Any)] -> [(Int,[(category,Any)],Int)]
  myResults = results
{-

(Table State (Tokens v0), Size)
tokens :: FT.Measured v IntToken => String -> v
tokens str = FT.measure $ stateToTree $ FT.measure $ makeTree str

tokensP :: FT.Measured v IntToken => String -> FT.FingerTree v IntToken
tokensP str = stateToTree $ FT.measure $ makeTree str

tokens' :: FT.Measured v IntToken => String -> (Table State (Tokens v), Size)
tokens' str = FT.measure $ makeTree str


mkTree :: RingP a => [Pair a] -> SomeTri a

-- From IncrementalCYKXXX
type ParseState = SomeTri [(CATEGORY,Any)]
parse :: FingerTree ParseState IntToken -> [(Int,[(CATEGORY,Any)],Int)]
parse tree = results $ measure tree


main = do
  f:_ <- getArgs
  s <- readFile f
  let ts = zipWith tokenToCats (cycle [False,True]) (Lexer.tokens s)
      (ts1,x:ts2) = splitAt (length ts `div` 2) ts
      cs = [mkTree ts1,mkTree' ts2]
      work [c1,c2] = show $ map fst $ root $ mergein False c1 x c2
  defaultMain [bench f $ nf work cs] -- note the hack!!!
-}
