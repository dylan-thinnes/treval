{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TrevalShowTypes where

import Data.Dynamic
import Debug.Trace

data Proxy (a :: k) = Proxy

data Witness a where
    ShowWitness :: Show a => a -> Witness a
    UnshowWitness :: a -> Witness a

fromDynamicShow :: (Show a, Typeable a) => Dynamic -> Maybe (a, String)
fromDynamicShow d = fmap (\x -> (x, show x)) $ fromDynamic $ traceShowId d

-- class MMShow a where
--     mmshow :: a -> Maybe String
-- 
-- instance (MShow 'True a) => MMShow a where
--     mmshow x = Just $ show x
