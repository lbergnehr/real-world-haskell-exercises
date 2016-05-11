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
-- hello
-- world
-- 
-- hw
-- eo
-- lr
-- ll
-- od
-- 
