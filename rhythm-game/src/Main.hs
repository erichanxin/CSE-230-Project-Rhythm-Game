module Main
  ( main
  ) where

import UI (mainGame)
import DeveloperUI (mainDeveloper)
import ChooseMode (chooseMode)
import Control.Monad (void,when)

repeatNTimes 0 _ = return ()
repeatNTimes n action =
 do
  action
  repeatNTimes (n-1) action

main :: IO ()
main = repeatNTimes 10 (do
    mode <- chooseMode
    Control.Monad.when (mode == 1) $ void mainGame
    Control.Monad.when (mode == 2) $ void mainDeveloper)