module Rhythm where
import Control.Lens hiding ((<|), (|>), (:>), (:<), Empty)

--- Game definitions: --
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
  } 

-- functions
initGame :: IO Game
initGame = do
  pure $
    Game { _song = [[10, 20, 30, 40],
                    [5, 20, 25, 30],
                    [15, 40],
                    [30, 50, 60, 90]]
        , _lastHit = Empty
        , _score = 0
        , _done = False
        }

fall :: [[Int]] -> [[Int]]
fall = map (filter (>0) . map (\h -> h-1))

step :: Game -> Game
step g = Game
    { _song       = fall (_song g)
    , _score      = _score g
    , _lastHit    = _lastHit g
    , _done       = _done g
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
      }