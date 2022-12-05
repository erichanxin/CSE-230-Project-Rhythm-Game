module UnitTests where
import Test.QuickCheck

import Rhythm

quickCheckN n = quickCheckWith (stdArgs {maxSuccess = n})
-- >>> quickCheckN 10000 prop_is_empty_song
-- +++ OK, passed 10000 tests.

prop_is_empty_song :: [[Int]] -> Bool
prop_is_empty_song s = isEmptySong s == ((length (concat s) == 0) && (length s == 4))

prop_fall_size :: [[Int]] -> Property
prop_fall_size s = 
    length s == 4 ==>   length (fall s) == 4 &&
                        length ((fall s)!!0) <= length (s!!0) && length ((fall s)!!1) <= length (s!!1) &&
                        length ((fall s)!!2) <= length (s!!2) && length ((fall s)!!3) <= length (s!!3)

prop_fall_positive :: [[Int]] -> Bool
prop_fall_positive s = all (>0) (concat (fall s))

prop_fall_one_unit :: [[Int]] -> Bool
prop_fall_one_unit s = all (`elem` (concat s)) (map (\h -> h+1) (concat (fall s)))

prop_evaluate_hit :: Int -> Bool
prop_evaluate_hit h = case evaluateHit h of
    (Miss, 0)       -> h > 5
    (Good, 3)       -> h /= 1 && h <= 5
    (Perfect, 5)    -> h == 1

-- >>> quickCheck prop_is_empty_song
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_evaluate_hit
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_fall_size
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_fall_positive
-- +++ OK, passed 100 tests.

-- >>> quickCheck prop_fall_one_unit
-- +++ OK, passed 100 tests.

-- >>> quickCheckN 1000 prop_fall_size
-- *** Gave up! Passed only 193 tests; 10000 discarded tests.