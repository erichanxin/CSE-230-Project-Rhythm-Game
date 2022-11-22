module Rhythm where
import Control.Lens hiding ((<|), (|>), (:>), (:<), Empty)

--- Game definitions: --
data Note = N { height :: Int }
  deriving (Eq, Ord)

data HitState
    = Perfect
    | Good
    | Miss
    | Empty
    deriving (Show, Eq, Ord)

-- Game State:
data Game = Game
  { _song       :: [[Int]]
  , _score      :: Int
  , _lastHit    :: HitState
  , _done       :: Bool
  } 


-- add options for bot later
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

step :: Game -> Game
step g = Game
    { _song       = map (map (\h -> h-1)) (_song g)
    , _score      = (_score g)
    , _lastHit    = (_lastHit g)
    , _done       = (_done g)
    } 