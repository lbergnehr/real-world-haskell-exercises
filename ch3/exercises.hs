import Data.List

-- 1. Length of a list
length' :: [a] -> Int
length' []      = 0
length' (x:xs)  = 1 + length' xs

-- 3. Mean of list
mean' :: [Float] -> Float
mean' [] = 0
mean' xs = (sum xs) / fromIntegral (length xs)

-- 4. Palindrome
toPalindrome :: [a] -> [a]
toPalindrome xs = xs ++ (reverse xs)

-- 5. Is Palindrome
isPalindrome :: [Int] -> Bool
isPalindrome xs = xs == (reverse xs)

-- 6. Sort lists
sortSublists :: [[Int]] -> [[Int]]
sortSublists = sortBy (\ a b -> compare (length a) (length b))

-- 7. Intersperse
intersperse' :: Char -> [String] -> String
intersperse' _ []      = ""
intersperse' _ [x]     = x
intersperse' c (x:xs)  = x ++ c:(intersperse' c xs)

-- 8. Tree depth
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

maxTreeDepth :: Tree a -> Int
maxTreeDepth Empty                = 0
maxTreeDepth (Node _ left right)  = 1 + max (maxTreeDepth left) (maxTreeDepth right)

-- 9. Turn
data Direction = Straight | Left | Right
  deriving (Show)


add :: Int -> Int -> Int
add a b = a + b


