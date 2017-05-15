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
-- import Algebra.RingUtils
import Algebra.RingUtils as RU
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
  showResults :: Int -> [(category,Any)] -> Int -> IO ()
  showResults f x t = do
        putStrLn $ show (length x) ++ " results"
        putStrLn $ show (f,t) ++ " (f,t)"
        forM_ x $ \(cat,ast) -> do
          putStrLn $ describe cat
          putStrLn $ showAst (cat,ast)

  runFile v f = putStrLn f >> readFile f >>= run f v
  run f v s =
    do case rs of
         [(f,x,t)] -> showResults f x t
         _ -> do let errs = pairs rs
                     best = minimum $ map quality errs
                 mapM_ (putStrLn . showErr ts) $ filter (\x -> quality x == best) errs
       when (v >= 2) $ do
         writeFile (f ++ ".xpm") (genXPM $ fingerprint chart)
         let scatt = scatterplot chart
         putStrLn $ "Scatterplot data size:" ++ show (length scatt)
         writeFile (f ++ ".data") scatt
         -- putStrLn $ "mat:" ++ (show linchart)
         writeFile (f ++ ".mat") (show linchart)
    where
          ts :: FingerTree (SomeTri [(category,Any)]) inttoken
          ts = myLLexer s
          chart :: SomeTri [(category, Any)]
          chart = myParser ts
          rs :: [(Int, [(category, Any)], Int)]
          rs = results chart

          ll :: SomeTri [(category,Any)] -> (String,[[[String]]],[[[String]]],[[[String]]])
          ll (T s (m :/: m')) = (show s, f m, f m', f (m RU.+ m'))
            where f n = (fmap $ fmap $ fmap (describe . fst)) $ lin s s n

          linchart :: (String, [[[String]]],[[[String]]],[[[String]]])
          linchart = ll chart

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
  showBestCat [] = "showBestCat: empty list"

{-
  mkMyPair :: [(category,Any)] -> Pair [(category,Any)]
  mkMyPair ts = f :/: b
    where
      (f,b) = splitAt (length ts `div` 2) ts

  mkMyTree :: [Pair [(category,Any)] ] -> SomeTri [(category,Any)]
  mkMyTree = mkTree

  myResults :: SomeTri [(category,Any)] -> [(Int,[(category,Any)],Int)]
  myResults = results
-}

