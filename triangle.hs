-- triangle support for prob18 and prob67

module Triangle where

type Triangle a = TriCell a -- root cell
data TriCell a = TriCell {
                   triValue :: a
                 , triLeft :: TriCell a
                 , triRight :: TriCell a
                 } | TriNull deriving (Show, Eq)

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

-- rewrite all values starting at the bottom
-- The given function takes the value of the cell, the value of the left branch,
-- and the value of the right branch, and spits out the new cell value.
-- The seed value is used in place of values for TriNull cells.
bubbleBy :: (a -> b -> b -> b) -> b -> Triangle a -> Triangle b
bubbleBy f seed TriNull = TriNull
bubbleBy f seed (TriCell a left right) = TriCell (f a leftVal rightVal) left' right'
  where left' = bubbleBy f seed left
        leftVal = case left' of
                    TriNull -> seed
                    (TriCell a _ _) -> a
        right' = bubbleBy f seed right
        rightVal = case right' of
                     TriNull -> seed
                     (TriCell a _ _) -> a

-- example bubble that produces the maximum value of a route from top to bottom
bubbleMax :: (Num a, Ord a) => Triangle a -> Triangle a
bubbleMax = bubbleBy (\a l r -> a + max l r) 0
