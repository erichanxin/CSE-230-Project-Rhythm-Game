module Main
  ( main
  ) where

import UI (mainGame)
import ChooseMode (chooseMode)
import Control.Monad (void,when)

main :: IO ()
main = do
  mode <- chooseMode
  Control.Monad.when (mode == 1) $ void mainGame