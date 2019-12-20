import Data.List (unfoldr)
import Data.Tuple (swap)

digits :: Integer -> [Integer]
digits = reverse . unfoldr (\x -> if x == 0 then Nothing else Just $ swap (divMod x 10))

main :: IO ()
main = print $ digits 1234
