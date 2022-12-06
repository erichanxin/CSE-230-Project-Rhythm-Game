import UnitTests
import Rhythm
import Test.QuickCheck

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
main = do
    quickCheckN 10000 prop_is_empty_song
    putStrLn "Test prop_is_empty_song"
    quickCheck prop_is_empty_song
    putStrLn "Test prop_fall_size"
    quickCheckN 1000 prop_fall_size
    putStrLn "Test prop_fall_positive"
    quickCheck prop_fall_positive
    putStrLn "Test prop_fall_one_unit"
    quickCheck prop_fall_one_unit
    putStrLn "Test prop_evaluate_hit"
    quickCheck prop_evaluate_hit
    putStrLn "Test prop_combo_counter_step"
    quickCheck prop_combo_counter_step
    putStrLn "Test prop_combo_counter_hit"
    quickCheck prop_combo_counter_hit