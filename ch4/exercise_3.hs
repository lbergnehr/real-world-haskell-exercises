import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = firstWords

-- 3.
firstWords :: String -> String
firstWords content = unlines $ map firstWord nonEmptyLines
  where firstWord     = head . words
        nonEmptyLines = filter (not . null) (lines content)

