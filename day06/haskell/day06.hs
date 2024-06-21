import Prelude.Math
--import Data.Array

parseFile :: FilePath -> IO [[Float]]
parseFile =
  fmap (map (map read . tail . words) . lines) . readFile

partTwo :: Float -> Float -> Int
partTwo time dist =
  let quadraticSolver pm =
        (pm (-time) (sqrt (time**2.0 + 4.0 * (-dist)))) / (-2.0)
      --s1 = read . Math.ceiling . quadraticSolver (+)
      --s2 = read . Math.ceiling . quadraticSolver (-)
  --in if s1 < s2 then s2 - s1 else s1 - s2
  in 0

main :: IO ()
main = do
  contents <- parseFile "../input_simple.txt"
  let times = contents !! 0
      dists = contents !! 1

  print $ contents
  print $ (times, dists)
