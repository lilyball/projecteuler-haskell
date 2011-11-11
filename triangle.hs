-- triangle support for prob18 and prob67

module Triangle where

type Triangle a = TriCell a -- root cell
data TriCell a = TriCell a (TriCell a) (TriCell a)
               | TriNull
                 deriving (Show, Eq)

triValue (TriCell a _ _) = a
triLeft  (TriCell _ l _) = l
triRight (TriCell _ _ r) = r

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
bubbleBy _ _ TriNull = TriNull
bubbleBy f seed tri = buildTriangle $ applyFunction rows
  where rows = [tri] : rows' (head rows)
        rows' ((TriCell _ TriNull _):_) = []
        rows' tss@(t:ts) = curRow : rows' curRow
          where curRow = triLeft t : map triRight tss
        applyFunction (as:[]) = [applySeed as]
          where applySeed [] = []
                applySeed (TriNull:_) = []
                applySeed (TriCell t _ _:ts) = f t seed seed : applySeed ts
        applyFunction (as:ass) = processRow as ass' : rest
          where rest = applyFunction ass
                ass' = head rest ++ repeat seed
                processRow [] _ = []
                processRow (TriNull:_) _ = []
                processRow (TriCell t _ _:ts) (a:as@(a':_)) = f t a a' : processRow ts as

-- example bubble that produces the maximum value of a route from top to bottom
bubbleMax :: (Num a, Ord a) => Triangle a -> Triangle a
bubbleMax = bubbleBy (\a l r -> a + max l r) 0
