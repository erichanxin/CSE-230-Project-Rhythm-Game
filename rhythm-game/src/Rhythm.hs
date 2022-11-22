module Rhythm where

--- Game definitions: --
data Note = N { height :: Int }
  deriving (Eq, Ord)

data HitState
    = Perfect
    | Good
    | Miss
    | Empty
    deriving (Eq, Ord)

-- Game State:
data Game = Game
  { _song       :: [[Note]]
  , _score      :: Int
  , _lastHit    :: HitState
  , _done       :: Bool
  } 


-- add options for bot later
initGame :: IO Game
initGame = do
  pure $
    Game { _song = [[N 10, N 20, N 30, N 40],
                    [N 5, N 20, N 25, N 30],
                    [N 15, N 40],
                    [N 30, N 50, N 60, N 90]]
        , _lastHit = Empty
        , _score = 0
        , _done = False
        }

step :: Game -> Game
step g = g