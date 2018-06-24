import qualified Data.Char as T
import Data.List

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

type Writer a = (a, String)

(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
(>=>) m1 m2 = \x ->
  let (y, s1) = m1 x
      (z, s2) = m2 y
  in (z, s1 ++ s2)

upCase :: String -> Writer String
upCase s = (map T.toUpper s, "upCase called|")

downCase :: String -> Writer String
downCase s = (map T.toLower s, "downCase called|")

toWords :: String -> Writer [String]
toWords s = (words s, "toWords called|")

toPhrase :: [String] -> Writer String
toPhrase s = (intercalate " " s, "toPhrase called|")

process :: String -> Writer [String]
process = upCase >=> toWords

reprocess :: String -> Writer String
reprocess = process >=> toPhrase
