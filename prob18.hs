-- By starting at the top of the triangle below and moving to adjacent numbers
-- on the row below, the maximum total from top to bottom is 23.
--
--                               3
--                              7 4
--                             2 4 6
--                            8 5 9 3
--
--                           (3 7 4 9)
--
-- That is, 3 + 7 + 4 + 9 = 23.
--
-- Find the maximum total from top to bottom of the triangle below:
--
--                              75
--                            95  64
--                           17 47 82
--                         18 35  87 10
--                        20 04 82 47 65
--                      19 01 23  75 03 34
--                     88 02 77 73 07 63 67
--                   99 65 04 28  06 16 70 92
--                  41 41 26 56 83 40 80 70 33
--                41 48 72 33 47  32 37 16 94 29
--               53 71 44 65 25 43 91 52 97 51 14
--             70 11 33 28 77 73  17 78 39 68 17 57
--            91 71 52 38 17 14 91 43 58 50 27 29 48
--           63 66 04 68 89 53 67  30 73 16 69 87 40 31
--          04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
--
-- NOTE: As there are only 16384 routes, it is possible to solve this problem by
-- trying every route. However, Problem 67, is the same challenge with a
-- triangle containing one-hundred rows; it cannot be solved by brute force,
-- and requires a clever method! ;o)
--

-- Let's try brute force here anyway, we can get the efficient version when we do prob 67

{------ Triangle ------}

type Triangle a = TriCell a -- root cell
data TriCell a = TriCell a (TriCell a) (TriCell a) | TriNull deriving (Show)

instance Functor TriCell where
  fmap f (TriCell a left right) = TriCell (f a) (fmap f left) (fmap f right)
  fmap f TriNull                = TriNull

buildTriangle :: [[a]] -> Triangle a
buildTriangle [] = TriNull
buildTriangle rows = head $ build 1 rows
  where build :: Int -> [[a]] -> [TriCell a]
        build min [] = replicate min TriNull
        build min (nums:lines) = cells ++ replicate (min - length cells) TriNull
          where cells = build' nums nextRow
                build' :: [a] -> [TriCell a] -> [TriCell a]
                build' [] _ = []
                build' (num:nums) (left:right:next) = TriCell num left right : build' nums (right:next)
                -- nextRow :: [TriCell a]
                -- Can't actually give it this type
                nextRow = build (succ $ length nums) lines

sampleTriangle :: Triangle Int
sampleTriangle = buildTriangle [[3],[7,4],[2,4,6],[8,5,9,3]]

{------ Pathfinding ------}

routes :: Triangle a -> [[TriCell a]]
routes TriNull = []
routes cell@(TriCell _ left right) = if null routes' then [[cell]] else routes'
  where routes' = [cell : route | route <- routes left ++ routes right]

sumRoute :: Num a => [TriCell a] -> a
sumRoute = sum . map cellValue
  where cellValue TriNull = 0
        cellValue (TriCell i _ _) = i

{------ IO ------}

parseFile :: IO [[Int]]
parseFile = return . map (map read . words) . lines =<< readFile "prob18.txt"

main = do
  lines <- parseFile
  let triangle = buildTriangle lines
  print $ maximum $ map sumRoute $ routes triangle
