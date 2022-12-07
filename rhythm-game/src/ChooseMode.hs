-- Functions for main menu UI and handleEvent
-- With "1" for game mode, "2" for developer mode, and "q" for quit  
module ChooseMode where

import System.Exit (exitSuccess)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui =
  padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ hLimit 50
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str " Choose Mode ")
    $ padTop (Pad 1)
    $ vBox $ map (uncurry drawKey)
    $ [ ("Game Mode", "1")
        , ("Developer Mode", "2")
        , ("Quit", "q")
        ]
    where
        drawKey act key = (padBottom (Pad 1) $ padRight Max $ padLeft (Pad 1) $ str act)
                          <+> (padBottom (Pad 1) $ padLeft Max $ padRight (Pad 1) $ str key)

handleEvent :: Maybe Int -> BrickEvent () e -> EventM () (Next (Maybe Int))
handleEvent n (VtyEvent (V.EvKey V.KEsc        _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt n
handleEvent n (VtyEvent (V.EvKey (V.KChar '1') [])) = halt $ Just 1
handleEvent n (VtyEvent (V.EvKey (V.KChar '2') [])) = halt $ Just 2
handleEvent n _ = continue n

chooseMode :: IO Int
chooseMode = defaultMain app Nothing >>= maybe exitSuccess return
