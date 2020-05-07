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
ftree = unsafePerformIO $ runQ qtree
    where
    qtree :: Q Exp
    qtree = join $ cataA f <$> [| x + y * 2 |]
    f :: ExpF (Q Exp) -> Q Exp
    f (VarEF name)        = VarE <$> newName (nameBase name)
    f (UnboundVarEF name) = UnboundVarE <$> newName (nameBase name)
    f (ConEF name)        = ConE <$> newName (nameBase name)
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

