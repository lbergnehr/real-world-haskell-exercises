import Data.Char (digitToInt)

-- 1.
-- Safe head
safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:_) = Just x

-- Safe tail
safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- Safe last
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

-- Safe init
safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- 2. Split with
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = 
  let (subArray, rest) = span f xs
  in  if null subArray
      then splitWith f (tail rest)
      else subArray : (splitWith f rest)

-- 3. See exercise_3.hs

-- 4. Transpose
transpose :: [String] -> [String]
transpose [] = []
transpose ls = headCol : transpose nonEmptyTailCols
  where headCol          = map head ls
        nonEmptyTailCols = filter (not . null) (map tail ls)

-- 2.1.
asInt_fold :: String -> Int
asInt_fold []                         = error "No digits entered"
asInt_fold ['-']                      = error "No digits entered"
asInt_fold ('-':str)                  = negate $ asInt_fold str
asInt_fold str       | '.' `elem` str = error "Only integers allowed"
                     | otherwise      = foldl ((+) . (*10)) 0 (map digitToInt str)

-- 2.2.
-- 2.3.
concat' :: [[a]] -> [a]
concat' lst = foldr step [] lst
  where step xs res = xs ++ res

-- 2.4.
takeWhile_expl :: (a -> Bool) -> [a] -> [a]
takeWhile_expl _ [] = []
takeWhile_expl f (x:xs) | f x       = x : takeWhile_expl f xs
                        | otherwise = []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f = foldr step []
  where step x xs = if f x then x : xs else []
