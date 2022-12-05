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
import System.Process (ProcessHandle, StdStream(CreatePipe,UseHandle),
  CreateProcess(std_err, std_out), createProcess, proc, terminateProcess)
import System.Process.Internals (ProcessHandle__(OpenHandle, ClosedHandle),
  withProcessHandle)
import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)
import System.Info
import System.Directory
import System.IO
import System.FilePath.Windows (FilePath)
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
  , _combo      :: Int
  , _maxCombo   :: Int
  } 

-- functions
initGame :: IO Game
initGame = do
  initMusic <- playMusic ("./assets" </> "temp_music.mp3")
  notes <- readNotes "noteLists.txt"
  pure $
    Game { _song = notes
        , _lastHit = Empty
        , _score = 0
        , _done = False
        , _musicHandle = initMusic
        , _combo = 0
        , _maxCombo = 0
        }


readNotes :: FilePath -> IO [[Int]]
readNotes path = do
  noteString <- readFile path 
  return $ read noteString

fall :: [[Int]] -> [[Int]]
fall = map (filter (>0) . map (\h -> h-1))

step :: Game -> Game
step g = do
  let newHit = if 1 `elem` concat (_song g) then Miss else _lastHit g
  Game
    { _song       = fall (_song g)
    , _score      = _score g
    , _lastHit    = newHit
    , _done       = null ((_song g)!!0) && null ((_song g)!!1) && null ((_song g)!!2) && null ((_song g)!!3)
    , _musicHandle = _musicHandle g
    , _combo      = if newHit == Miss then 0 else _combo g
    , _maxCombo   = _maxCombo g
    } 

hit :: HitKey -> Game -> Game
hit k g = do
  let n = keyToCol k
  let s = _song g
  if length (s!!n) == 0 then g else do
    let height = head (s!!n)
    let newHit = if height == 1 then Perfect else (if height > 5 then Miss else Good)
    let newCombo = if newHit == Miss then 0 else (_combo g + 1)
    Game
      { _song       = if newHit == Miss then s else fall (s & element n .~ tail (s!!n))
      , _score      = _score g + (if newHit == Perfect then 5 else (if newHit == Miss then 0 else 3))
      , _lastHit    = newHit
      , _done       = _done g
      , _musicHandle = _musicHandle g
      , _combo      = newCombo
      , _maxCombo   = max newCombo (_maxCombo g)
      }

playMusic :: FilePath -> IO ProcessHandle
playMusic path = withFile "/dev/null" WriteMode $ \hd_ -> do
  case os of "darwin" ->  do (_, _, _, processHandle) <- createProcess (proc "mpg321" [path]) {
        std_out = UseHandle hd_
        ,std_err = UseHandle hd_
      }                      
                             return processHandle
             "linux" ->   do (_, _, _, processHandle) <- createProcess (proc "mpg321" [path]) {
        std_out = UseHandle hd_
        ,std_err = UseHandle hd_
      }
                             return processHandle
             "mingw32" -> do (_, _, _, processHandle) <- createProcess (proc "start" [path]) {
        std_out = UseHandle hd_
        ,std_err = UseHandle hd_
      }
                             return processHandle

stopMusic :: ProcessHandle -> IO ()
stopMusic = terminateProcess

quitGame :: Game -> EventM Name (Next Game)
quitGame g = do
  liftIO $ stopMusic (_musicHandle g)
  halt g

restartGame :: Game -> EventM Name (Next Game)
restartGame g = do
  liftIO $ stopMusic (_musicHandle g)
  liftIO (initGame) >>= continue