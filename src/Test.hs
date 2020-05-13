{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
module Test where

import TrevalTH
import TrevalMonad --hiding (zoomWithTrace)
import Debug.Trace
import System.IO.Unsafe

import Control.Lens

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax (lift)

import Control.Monad.State.Class hiding (lift)
import Control.Monad.State.Lazy hiding (lift)
import Control.Monad.RWS.Lazy hiding (lift)

--zoomWithTrace :: Traversal' Exp Exp
--              -> TracingState (Either String Exp) c
--              -> TracingState (Either String Exp) c
--zoomWithTrace lens m = do
--    state <- get
--    censor (map $ \substate -> _Right . lens .~ substate $ state)
--        $ zoom (_Right . lens)
--        $ m

f, f' :: Int -> Int -> Int -> Int
f x y z = x + y * 2
f' x y z = traceShowId (traceShowId x + traceShowId (traceShowId y * 2))

ftree :: Exp
ftree = unsafePerformIO $ runQ $ [| x + y * 2 |] >>= flattenNames

flattenNames :: Exp -> Q Exp
flattenNames = cataA f
    where
    f :: ExpF (Q Exp) -> Q Exp
    f (VarEF name)        = VarE <$> return (mkName (nameBase name))
    f (UnboundVarEF name) = UnboundVarE <$> return (mkName (nameBase name))
    f (ConEF name)        = ConE <$> return (mkName (nameBase name))
    f x                   = fmap embed $ sequence x

ftrace :: Integer -> Integer -> Integer -> TracingState Exp ()
ftrace x y z = do
    commit
    x <- zoomWithTrace (_InfixE . _1 . _Just) $ do
        put $ LitE $ IntegerL x
        commit
        return [x]
    yt2 <- zoomWithTrace (_InfixE . _3 . _Just) $ do
        y <- zoomWithTrace (_InfixE . _1 . _Just) $ do
            put $ LitE $ IntegerL y
            commit
            return [y]
        let [y'] = y
        put $ LitE $ IntegerL $ y' * 2
        commit
        return [y' * 2]
    let [x'] = x
    let [yt2'] = yt2
    put $ LitE $ IntegerL $ x' + yt2'
    commit
    pure ()

ftraceX :: Integer -> TracingStateT Exp Maybe [Integer]
ftraceX x = do
    let res = x
    put $ LitE $ IntegerL res
    commit
    return [res]

ftraceY :: Integer -> TracingStateT Exp Maybe [Integer]
ftraceY y = do
    let res = y
    put $ LitE $ IntegerL res
    commit
    return [res]

ftraceZ :: Integer -> TracingStateT Exp Maybe [Integer]
ftraceZ z = do
    let res = z
    put $ LitE $ IntegerL res
    commit
    return [res]

ftraceYT2 :: Integer -> TracingStateT Exp Maybe [Integer]
ftraceYT2 y = do
    [y] <- zoomWithTrace (_InfixE . _1 . _Just) (ftraceY y)
    let res = y * 2
    put $ LitE $ IntegerL res
    commit
    return [res]

ftrace' :: Integer -> Integer -> Integer -> TracingStateT Exp Maybe [Integer]
ftrace' x y z = do
    commit
    [x]   <- zoomWithTrace (_InfixE . _1 . _Just) (ftraceX x)
    [yt2] <- zoomWithTrace (_InfixE . _3 . _Just) (ftraceYT2 y)
    let res = x + yt2
    put $ LitE $ IntegerL res
    commit
    return [res]

type TMI = TracingStateT Exp Maybe [Integer]

fold' :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
fold' f x [] = x
fold' f x (y:xs) = fold' f (f x y) xs

foldingTrace :: (Integer -> Integer -> TMI)
             -> Integer
             -> [Integer]
             -> TMI
foldingTrace f seed [] = do
    let res = seed
    put $ LitE $ IntegerL res
    commit
    return [res]

