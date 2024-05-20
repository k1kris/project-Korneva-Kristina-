module Pure
    ( makeServices, makeBusy, makeFree, checkNameSurname, checkDate, 
         asc, returnDate, returnDay, returnTime, findSpecialist, checkDateFile
    ) where

import Types(Specialist(..), Day(..), Time(..), Services(..), Dates(..))

import Data.List.Split(splitOn) --for splitting the file
import Data.List(delete, sortOn)
{-for deleting an element from the list and for the sorting out the list-}

{-Function to create a list of services that are available for the user:
as services are numbered in the file 
we analyzing the content by finding "1)" etc-}

makeServices :: [String] -> Services
makeServices [] = Services []
makeServices (str:xs)
    |(str !! 1) == ')' = mappend (Services ((delete '\r' (delete ' ' 
	     (delete ')' (delete (str !! 0) str)))):[])) (makeServices xs)
    | otherwise = (makeServices xs)

{-Function to make the list of busy dates-}

makeBusy :: [String] -> Dates
makeBusy [] = Dates []
makeBusy (str:xs)
{-through the analyzing the requests in the file 
which are written on the strings before services-}
    | (str !! 0) /= '1' = mappend (Dates [(returnDate [((splitOn " " str) !! 3), 
	     ((splitOn " " str) !! 4)])]) (makeBusy xs)
    | otherwise = Dates []

{-Function to create the list of free dated using the list of busy dates-}

makeFree :: [(Time,Day)] -> [(Time,Day)]
makeFree busy = filter (\x->not (elem x busy)) 
     [(z,y) | z <- [AM9, AM10, AM11, PM12, PM13, PM14, PM15, PM16, PM17], 
	      y <- [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]]
{-using "filter" we delete elements that are in the busy list 
from the generated list of all possible dates-}

{-Function to check whether or not user's name 
and surname are in the content of the file-}

checkNameSurname :: [String] -> [String] -> Bool
checkNameSurname [] _ = False
checkNameSurname [_] _ = False
checkNameSurname (_:_:_:_) _ = False
checkNameSurname [_, _] [] = False
checkNameSurname [n, s] (lst:xs) 
    | (n == ((splitOn " " lst) !! 0) && s == ((splitOn " " lst) !! 1)) 
	     == True = True
    | (lst !! 0) == '1' = False
    | otherwise = checkNameSurname [n, s] xs

{-Function to check whether or not 
user's date are in the content of the specialist's schedule-}

checkDate :: [String] -> Specialist -> Bool
checkDate [t, d] (Specialist _ busy _ _) 
     = elem (returnDate [t, d]) (getDates busy)

{-Function to check whether or not 
user's date are in the content of the file-}

checkDateFile :: [String] -> [String] -> Bool
checkDateFile [] _ = False
checkDateFile [_] _ = False
checkDateFile (_:_:_:_) _ = False
checkDateFile [_, _] [] = False
checkDateFile [t, d] (str:xs)
    | (t == ((splitOn " " str) !! 3)) && (d == ((splitOn " " str) !! 4)) 
	     == True = True
	| (str !! 0) == '1' = False
	| otherwise = checkDateFile [t, d] xs

{-Function to sort out the list of free dates in ascending order-}

asc :: [(Time, Day)] -> [(Time, Day)]
asc lst = sortOn snd(sortOn fst lst)

{-Function to make from a string the date in new types Time and Day-}

returnDate :: [String] -> (Time, Day)
--returnDate [] = ()
--returnDate [_] = ()
--returnDate (_:_:_:_) = ()
returnDate [x1, x2] = (returnTime x1, returnDay x2)

{-Function to make from string words a Time constructor-}

returnTime :: String -> Time
returnTime t | t == "9" = AM9
             | t == "10" = AM10
             | t == "11" = AM11
             | t == "12" = PM12
             | t == "13" = PM13
             | t == "14" = PM14
             | t == "15" = PM15
             | t == "16" = PM16
             | otherwise = PM17

{-Function to make from string words a Day constructor-}

returnDay :: String -> Day
returnDay t | t == "Monday" = Monday
            | t == "Tuesday" = Tuesday
            | t == "Wednesday" = Wednesday
            | t == "Thursday" = Thursday
            | t == "Friday" = Friday
            | otherwise = Saturday

{-Function to find a specialist with chosen service-}

findSpecialist :: String -> [Specialist] -> Specialist
findSpecialist serv (Specialist f b fr servs:xs)
    | elem serv (getServices servs) = (Specialist f b fr servs)
	| otherwise = findSpecialist serv xs