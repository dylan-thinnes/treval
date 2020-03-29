{-# LANGUAGE PackageImports #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TrevalMonad where

import TrevalTH

import Control.Lens
import Control.Lens.Internal.Zoom

import Control.Monad.State.Class hiding (lift)
import Control.Monad.State.Strict hiding (lift)
import Control.Monad.RWS.Strict hiding (lift)

import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax (lift)

import Data.Functor.Identity
import Data.Functor.Compose

import Debug.Trace

-- A version of Exp/ExpF that also can have "holes" for arbitrary string expressions
-- This will be used for reporting later, for now we wrap shown values in LitE
type HoledF = Compose (Either String) ExpF
type Holed = Fix HoledF

-- A RWS monad with a "commit" that can save the current state to the trace
type TracingStateT s m a = RWST () [s] s m a

commit :: (Monad m) => TracingStateT s m ()
commit = get >>= tell . pure

runTracingT :: TracingStateT s m a -> s -> m (a, s, [s])
runTracingT t = runRWST t ()

runTracing :: TracingStateT s Identity a -> s -> (a, s, [s])
runTracing t = runIdentity . runTracingT t

type TracingState s a = TracingStateT s Identity a

{-
-- A State monad with a "commit" that can save the current state to a trace
newtype TracingStateT s m a = TracingStateT (StateT ([s], s) m a)
    deriving (Functor, Applicative, Monad)

instance Monad m => MonadState s (TracingStateT s m) where
    get = TracingStateT $ zoom _2 get
    put x = TracingStateT $ zoom _2 (put x)
    state f = TracingStateT $ zoom _2 (state f)

commit :: (Monad m) => TracingStateT s m ()
commit = TracingStateT $ do
    state <- gets snd
    modify (_1 %~ (state:))

-- Runner helpers
runTracingT :: (Monad m) => TracingStateT s m a -> s -> m (a, [s], s)
runTracingT (TracingStateT m) initial = do
    (a, (trace, s)) <- runStateT m ([], initial)
    return (a, trace, s)

type TracingState s a = TracingStateT s Identity a

runTracing :: TracingStateT s Identity a -> s -> (a, [s], s)
runTracing (TracingStateT m) initial = runIdentity $ do
    (a, (trace, s)) <- runStateT m ([], initial)
    return (a, trace, s)
-}

-- Function for checking whether a type has a Show instance
isShowable :: Type -> Q Bool
isShowable t = isInstance ''Show [traceShowId $ t]

-- Isomorphic to Maybe Bool
data Lifted = Yes | No | Unsure
    deriving (Show, Eq, Ord)

-- Turns computations into traces
tracer :: Exp -> Q Exp
tracer = fmap snd . Data.Functor.Foldable.para f
    where
    unsure :: ExpF (Exp, Q (Lifted, Exp)) -> Q (Lifted, Exp)
    unsure = fmap ((Unsure,) . embed)
           . sequence 
           . fmap (fmap snd . snd)


    orig :: ExpF (Exp, a) -> Exp
    orig x = embed (fst <$> x)

    f :: ExpF (Exp, Q (Lifted, Exp)) -> Q (Lifted, Exp)
    f x@(AppEF (funExprOrig, funExprQ) (argExprOrig, argExprQ))
        = do
            let original = orig x
            (funExprLifted, funExpr) <- funExprQ
            (argExprLifted, argExpr) <- argExprQ
            fmap (Yes,) $
                  [| do
                        put original
                        commit
                        fun <- do
                            state <- get
                            censor (map $ \substate -> _AppE . _1 .~ substate $ state)
                                $ zoom (_AppE . _1)
                                $ $(return funExpr)
                        arg <- do
                            state <- get
                            censor (map $ \substate -> _AppE . _2 .~ substate $ state)
                                $ zoom (_AppE . _2)
                                $ $(return argExpr)
                        let result = fun <*> arg
                        return result
                  |]
    f x@(VarEF name) = reify name >>= \case
            (VarI _ _type _)
              -> isShowable _type >>= \(traceShowId -> isShowable) ->
                    let original = orig x
                     in if isShowable
                        then fmap (Yes,)
                               [| do
                                      put original
                                      commit
                                      let result = $(pure original)
                                      put $ LitE $ StringL $ show result
                                      commit
                                      return $ First $ Just result
                               |]
                        else fmap (Yes,) [| return $ First $ Just $(pure original) |]
            _
              -> fmap ((Unsure,) . embed) $ sequence $ fmap (fmap snd . snd) x
    f x = unsure x

-- testing
f = (pure :: a -> TracingState Exp a) ((+) :: Int -> Int -> Int)
g :: Int -> Int -> Int
g = (*)
x :: Int
x = 3
y :: Int
y = 2
--traceF = $(tracer =<< [| f 1 2 |])


