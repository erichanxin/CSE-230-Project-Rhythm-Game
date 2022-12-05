module Main
  ( main
  ) where

import UI (mainGame)
import DeveloperUI (mainDeveloper)
import ChooseMode (chooseMode)
import Control.Monad (void,when,replicateM_)

main :: IO ()
main = replicateM_ 10 (do
    mode <- chooseMode
    Control.Monad.when (mode == 1) $ void mainGame
    Control.Monad.when (mode == 2) $ void mainDeveloper)