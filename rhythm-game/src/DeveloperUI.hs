module DeveloperUI where

import DeveloperMode

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor, attrName, simpleMain
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, padBottom, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Util as U
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
-- marks passing of time
data Tick = Tick

data Col = FirstCol | SecondCol | ThirdCol | FourthCol | EmptyCol | Bottom


-- define App
app :: App Developer Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- color attributes:

mainDeveloper :: IO ()
mainDeveloper = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  d <- initDeveloper
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app d


handleEvent :: Developer -> BrickEvent Name Tick -> EventM Name (Next Developer)
handleEvent d (AppEvent Tick)                       = continue $ step d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ hit KeyA d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ hit KeyD d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ hit KeyJ d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ hit KeyL d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'r') [])) = restartDeveloper d
handleEvent d (VtyEvent (V.EvKey (V.KChar 'q') [])) = quitDeveloper d
handleEvent d _                                     = continue d

-- Drawing
drawUI :: Developer -> [Widget Name]
drawUI d =
  [ C.vCenter $ hBox $ 
  [padRight (Pad 4) (drawStats d),
  drawSong d,
  padLeft (Pad 4) $ vBox $ [drawInfo]
  ]]

drawSong :: Developer -> Widget Name
drawSong d = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Notes ")
  $ vBox $ map (uncurry drawKey)
  $ [ ("A ", show $ showList 0)
    , ("D ", show $ showList 1)
    , ("J ", show $ showList 2)
    , ("L ", show $ showList 3)
    ]
    where
    drawKey act key = (padBottom (Pad 1) $ padRight Max $ padLeft (Pad 1) $ str act)
                      <+> (padBottom (Pad 1) $ padLeft Max $ padRight (Pad 1) $ str key)
    showList n = if (length $ (_song d)!!n) < 6 then show $ (_song d)!!n else
                    "..." ++ (show $ reverse . take 6 . reverse $ (_song d)!!n)

drawInfo :: Widget Name
drawInfo = withBorderStyle BS.unicodeBold
  $ hLimit 25
  $ B.borderWithLabel (str " Commands ")
  $ vBox $ map (uncurry drawKey)
  $ [ ("Hit Blue ", "A")
    , ("Hit Red ", "D")
    , ("Hit Green", "J")
    , ("Hit Yellow ", "L")
    , ("Restart ", "r")
    , ("Quit", "q")
    ]
  where
    drawKey act key = (padBottom (Pad 1) $ padRight Max $ padLeft (Pad 1) $ str act)
                      <+> (padBottom (Pad 1) $ padLeft Max $ padRight (Pad 1) $ str key)

drawStats :: Developer -> Widget Name
drawStats d = hLimit 11
  $ vBox [ drawClock (_clock d)]

drawClock :: Int -> Widget Name
drawClock n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Clock ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ 
  ]