module Rhythm where
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
import Control.Lens hiding ((<|), (|>), (:>), (:<), Empty)
import System.Process (ProcessHandle, StdStream(CreatePipe),
  CreateProcess(std_err), createProcess, proc, terminateProcess)
import System.Posix (ProcessID)
import System.Posix.Signals (signalProcess)
import System.Process.Internals (ProcessHandle__(OpenHandle, ClosedHandle),
  withProcessHandle)
import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)

--- Game definitions: --

type Name = ()

data Note = N { height :: Int }
  deriving (Eq, Ord)

data HitState
    = Perfect | Good | Miss | Empty
    deriving (Show, Eq, Ord)

data HitKey
    = KeyA | KeyD | KeyJ | KeyL
    deriving (Show, Eq, Ord)

keyToCol :: HitKey -> Int
keyToCol KeyA = 0
keyToCol KeyD = 1
keyToCol KeyJ = 2
keyToCol KeyL = 3

-- Game State:
data Game = Game
  { _song       :: [[Int]]
  , _score      :: Int
  , _lastHit    :: HitState
  , _done       :: Bool
  , _musicHandle :: ProcessHandle
  } 

-- functions
initGame :: IO Game
initGame = do
  initMusic <- playMusic ("../assets" </> "temp_music.mp3")
  pure $
    Game { _song = [[40, 75, 80, 120],
                    [35, 45, 70, 90, 100],
                    [50, 65, 100],
                    [55, 60, 80, 110]]
        , _lastHit = Empty
        , _score = 0
        , _done = False
        , _musicHandle = initMusic
        }


fall :: [[Int]] -> [[Int]]
fall = map (filter (>0) . map (\h -> h-1))

step :: Game -> Game
step g = Game
    { _song       = fall (_song g)
    , _score      = _score g
    , _lastHit    = if 1 `elem` concat (_song g) then Miss else _lastHit g
    , _done       = _done g
    , _musicHandle = _musicHandle g
    } 

hit :: HitKey -> Game -> Game
hit k g = do
  let n = keyToCol k
  let s = _song g
  if length (s!!n) == 0 then g else do
    let height = head (s!!n)
    Game
      { _song       = fall (s & element n .~ tail (s!!n))
      , _score      = _score g + (if height == 1 then 5 else (if height > 3  then 0 else 3))
      , _lastHit    = if height == 1 then Perfect else (if height > 3 then Miss else Good)
      , _done       = _done g
      , _musicHandle = _musicHandle g
      }

playMusic :: FilePath -> IO ProcessHandle
playMusic path = do
  (_, _, _, processHandle) <-
    createProcess (proc "afplay" [path]) {
        std_err = CreatePipe
      }
  return processHandle

stopMusic :: ProcessHandle -> IO ()
stopMusic = terminateProcess

quitGame :: Game -> EventM Name (Next Game)
quitGame g = do
  liftIO $ stopMusic (_musicHandle g)
  halt g