--File: main.hs
--ghc --make main.hs

import Data.List
import System.IO

main = do 
    putStrLn "What's your name"
    name <- getLine
    putStrLn ("Hello " ++ name)
