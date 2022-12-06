module UI where

import Rhythm

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

data Col = FirstCol | SecondCol | ThirdCol | FourthCol | EmptyCol | Bottom | HitBottom


-- define App
app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

-- color attributes:

mainGame :: IO ()
mainGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g


handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = step g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'a') [])) = continue $ hit KeyA g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'd') [])) = continue $ hit KeyD g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ hit KeyJ g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ hit KeyL g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = quitGame g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = restartGame g
handleEvent g _                                     = continue g

-- Drawing
drawUI :: Game -> [Widget Name]
drawUI g =
  if (_done g) then [C.vCenter $ C.hCenter $ drawGameOver g] else (
    [ C.vCenter $ hBox $ 
    [padRight (Pad 4) (drawStats g),
    drawGrid g,
    padLeft (Pad 4) $ vBox $ [drawInfo, padTop (Pad 2) $ drawLastHit g ]
    ]]
  )

drawGameOver :: Game -> Widget Name
drawGameOver g = withBorderStyle BS.unicodeBold
  $ hLimit 100
  $ B.borderWithLabel (str " Game over ")
  $ vBox $ [str "     Game over     "
  , str ("     Final score: "++ (show $ (_score g)))
  , str ("     Maximum combo: "++ (show $ (_maxCombo g)))
  , str ("     Press Q to go back to main page or press R to restrart.     ")
  ]


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

drawLastHit :: Game -> Widget Name
drawLastHit g = withBorderStyle BS.unicodeBold
  $ hLimit 25
  $ B.borderWithLabel (str " Last Hit ")
  $ C.hCenter
  $ padAll 1
  $ str $ show (_lastHit g)

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (_score g), drawCombo (_combo g)]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Score ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawCombo :: Int -> Widget Name
drawCombo n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Combo ")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str " Rhythm ")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [39, 38..0]]
    cellsInRow y = [(drawCoord x y) | x <- [0..40]]
    drawCoord x y
        | y == 1                      = if (x `div` 10) == (_lastHitCol g) then drawCell HitBottom else drawCell Bottom
        | x >= 1 && x < 9 && y `elem` (_song g)!!0       = drawCell FirstCol
        | x >= 11 && x < 19 && y `elem` (_song g)!!1     = drawCell SecondCol
        | x >= 21 && x < 29 && y `elem` (_song g)!!2     = drawCell ThirdCol
        | x >= 31 && x < 39 && y `elem` (_song g)!!3     = drawCell FourthCol
        | otherwise             = drawCell EmptyCol

drawCell :: Col -> Widget Name
drawCell FirstCol = withAttr firstAttr cw
drawCell SecondCol  = withAttr secondAttr cw
drawCell ThirdCol = withAttr thirdAttr cw
drawCell FourthCol = withAttr fourthAttr cw
drawCell EmptyCol = withAttr emptyAttr cw
drawCell Bottom = withAttr bottomAttr cwBottom
drawCell HitBottom = withAttr hitBottomAttr cwHitBottom

cw :: Widget Name
cw = str "  "

cwBottom :: Widget Name
cwBottom = str "--"

cwHitBottom :: Widget Name
cwHitBottom = str "||"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (firstAttr, V.blue `on` V.blue)
  , (secondAttr, V.red `on` V.red)
  , (thirdAttr, V.green `on` V.green)
  , (fourthAttr, V.yellow `on` V.yellow)
  , (bottomAttr, V.cyan `on` V.black)
  , (hitBottomAttr, V.brightCyan `on` V.black)
  ]

firstAttr, secondAttr, thirdAttr, fourthAttr :: AttrName
firstAttr = attrName "firstAttr"
secondAttr = attrName "secondAttr"
thirdAttr = attrName "thirdAttr"
fourthAttr = attrName "fourthAttr"
emptyAttr = attrName "emptyAttr"
bottomAttr = attrName "bottomAttr"
hitBottomAttr = attrName "hitBottomAttr"

