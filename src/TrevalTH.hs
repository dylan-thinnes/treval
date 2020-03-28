{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}-}

module TrevalTH where

import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Syntax
import Control.Lens
import Control.Lens.TH
import Data.Functor.Foldable.TH

makeBaseFunctor ''Exp
makePrisms ''ExpF
makePrisms ''Exp

deriving instance Lift AnnTarget
deriving instance Lift RuleBndr
deriving instance Lift InjectivityAnn
deriving instance Lift Phases
deriving instance Lift FamilyResultSig
deriving instance Lift RuleMatch
deriving instance Lift Inline
deriving instance Lift FixityDirection
deriving instance Lift Safety
deriving instance Lift Callconv
deriving instance Lift SourceStrictness
deriving instance Lift SourceUnpackedness
deriving instance Lift PkgName
deriving instance Lift NameSpace
deriving instance Lift PatSynDir
deriving instance Lift PatSynArgs
deriving instance Lift DerivStrategy
deriving instance Lift Role
deriving instance Lift TypeFamilyHead
deriving instance Lift TySynEqn
deriving instance Lift Pragma
deriving instance Lift Fixity
deriving instance Lift Foreign
deriving instance Lift Overlap
deriving instance Lift FunDep
deriving instance Lift Bang
deriving instance Lift ModName
deriving instance Lift DerivClause
deriving instance Lift Con
deriving instance Lift Clause
deriving instance Lift Body
deriving instance Lift TyLit
deriving instance Lift TyVarBndr
deriving instance Lift NameFlavour
deriving instance Lift OccName
deriving instance Lift Range
deriving instance Lift Stmt
deriving instance Lift Dec
deriving instance Lift Guard
deriving instance Lift Match
deriving instance Lift Pat
deriving instance Lift Type
deriving instance Lift Lit
deriving instance Lift Name
deriving instance Lift Exp

{-
getName :: Exp -> Maybe Name
getName (VarE name)         = Just name
getName (ConE name)         = Just name
getName (UnboundVarE name)  = Just name
getName _                   = Nothing

-- Using hoist to easily clean names in any part of the tree
cleanNames :: Exp -> Exp
cleanNames = hoist f
    where
    f :: ExpF a -> ExpF a
    f (VarEF name) = VarEF (mkName $ nameBase name)
    f (ConEF name) = ConEF (mkName $ nameBase name)
    f x            = x

-- Example tracer
tracer :: Exp -> Exp
tracer exp@(VarE name)
    = AppE (AppE (VarE $ mkName "Debug.Trace.trace")
                 (LitE $ StringL $ pprint $ cleanNames exp))
           exp
tracer exp@(ConE name)
    = AppE (AppE (VarE $ mkName "Debug.Trace.trace")
                 (LitE $ StringL $ pprint $ cleanNames exp))
           exp
tracer exp = embed . fmap tracer . project $ exp

-- Example code
f :: Int -> Int
f = (+1)

g :: Int -> Int -> Int
g = (+)
-}
