{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Treval (plugin, Treval(..)) where

import GhcPlugins
import Data.Data

data Treval = Treval deriving Data

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: CorePlugin
install _ todos = pure $ CoreDoPluginPass "Print Trevals" treval : todos

treval :: ModGuts -> CoreM ModGuts
treval guts = do
    dflags <- getDynFlags
    undefined


