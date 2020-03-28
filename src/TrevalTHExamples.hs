{-# LANGUAGE TemplateHaskell #-}

module TrevalTHExamples where

import TrevalTH
import Debug.Trace

-- foldrTrace :: (a -> b -> b) -> b -> [a] -> b
-- foldrTrace f acc [] = $(tracer <$> [| acc |])
-- foldrTrace f acc (x:xs) = $(tracer <$> [| foldr f (f x acc) xs |])

