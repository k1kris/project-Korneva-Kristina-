{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Types(Specialist(..), Dates(..))
import Lib(res1, res2, res3)
import Pure(makeBusy, makeFree, makeServices)

{-This function gives a user an option between
deleting his current request or creating a new one-}

main :: IO()
main =  do
    file1 <- readFile "Petrova.txt"
    let specialist1 = Specialist "specialist1" (makeBusy (lines file1))
         (Dates (makeFree (getDates (makeBusy (lines file1)))))
             (makeServices (lines file1))
    file2 <- readFile "Ivanova.txt"
    let specialist2 = Specialist "specialist2" (makeBusy (lines file2))
         (Dates (makeFree (getDates (makeBusy (lines file2)))))
             (makeServices (lines file2))
    print("Hello! You would like 1) create a new appointment, \ 
	     \2) delete you current one \
		     \or 3) look at the specialist's schedule:")
    res <- getLine--Read user's input
    if res == "1" then do
        res1 [specialist1, specialist2]--To create new one
    else do
        if res == "2" then
            res2 [file1, file2] --To delete
        else
            res3 [specialist1, specialist2] --To look at schedule
