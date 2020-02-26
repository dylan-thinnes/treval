{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PackageImports #-}

module TrevalTH where

import "template-haskell" Language.Haskell.TH
import Debug.Trace
import Control.Monad
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

makeBaseFunctor ''Exp

getName :: Exp -> Maybe Name
getName (VarE name)         = Just name
getName (ConE name)         = Just name
getName (UnboundVarE name)  = Just name
getName _                   = Nothing

changeNames :: Exp -> Exp
changeNames = hoist f
    where
    f :: ExpF a -> ExpF a
    f (VarEF name) = VarEF (mkName $ nameBase name)
    f (ConEF name) = ConEF (mkName $ nameBase name)
    f x            = x

f :: Int -> Int
f = (+1)

tracer :: Q Exp -> Q Exp
tracer = liftM addPrint
    where
    addPrint :: Exp -> Exp
    addPrint exp@(AppE (getName -> Just name) _)
        = AppE (AppE (VarE $ mkName "Debug.Trace.trace")
                     (LitE $ StringL $ pprint exp))
               exp
    addPrint exp = exp
