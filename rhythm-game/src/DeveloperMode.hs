module DeveloperMode where
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
import Control.Monad (when)
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

-- Developer State:
data Developer = Developer
  { _song        :: [[Int]]
  , _clock       :: Int
  , _musicHandle :: ProcessHandle
  } 

-- functions
initDeveloper :: IO Developer
initDeveloper = do
  initMusic <- playMusic ("./assets" </> "temp_music.mp3")
  pure $
    Developer { _song = [[],
                    [],
                    [],
                    []]
                , _clock = 0
                , _musicHandle = initMusic
              }

step :: Developer -> Developer
step d = Developer
    { _song       = _song d
    , _clock      = (_clock d) + 1
    , _musicHandle = _musicHandle d
    } 

hit :: HitKey -> Developer -> Developer
hit k d = do
  let n = keyToCol k
  let s = _song d
  let c = _clock d
  Developer
    { _song       = s & element n .~ (s!!n ++ [c])
    , _clock      = (_clock d) + 1
    , _musicHandle = _musicHandle d
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

writeToFile :: Developer -> IO ()
writeToFile d = writeFile "noteLists.txt" (show $ _song d)

quitDeveloper :: Developer -> EventM Name (Next Developer)
quitDeveloper d = do
  liftIO $ stopMusic (_musicHandle d)
  liftIO $ writeToFile d
  halt d

restartDeveloper :: Developer -> EventM Name (Next Developer)
restartDeveloper d = do
  liftIO $ stopMusic (_musicHandle d)
  liftIO (initDeveloper) >>= continue