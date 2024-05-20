{-# LANGUAGE BlockArguments #-}

module Lib
    ( res1, res2, res3, chooseService, checkService, getTimeDay, 
         deleteRequest, makeAppointment, showAppointment
    ) where

import Pure(makeBusy, checkNameSurname, 
    checkDate, asc, returnDate, findSpecialist, checkDateFile)
import Types(Person(..), Appointment(..), Specialist(..), Day(..), 
    Time(..), Services(..), Dates(..))

import Data.List.Split(splitOn) --for splitting the file

{-Function when creating a new request to file out the Person data
and then check or choode the service-}

res1 :: [Specialist] -> IO()
res1 specialists = do
    print("PLease, write your name, surname and contact(telephone number \
	     \or e-mail) separated by spaces:")
    naming <- getLine
    print("If you have a preffered time and day then write it in the format \
	     \- 10 Monday. If you do not have then write No:")
    dating <- getLine
    print("If you have a preffered service then write it in the format - \
	     \Certification of will 2400. If you do not have then write No:")
    serv <- getLine
    if
        serv == "No"
    then
        chooseService naming dating specialists
    else
        checkService naming dating serv specialists

{-Function to choose service-}

chooseService :: String -> String -> [Specialist] -> IO()
chooseService ns dt specialists = do
    let listServices = (concat (map getServices (map services specialists)))
    {-the list of the services tha are available-}
    print("Choose service from this list: " ++ show listServices)
    ans <- getLine --the answer for the question of choosing the service
    let spec = findSpecialist ans specialists
    if dt == "No" then
        makeAppointment True ns ((asc (getDates (freeDates spec))) !! 0) ans
    else
        if checkDate (splitOn " " dt) spec then
            makeAppointment True ns ((asc (getDates (freeDates spec))) !! 0) 
                 ans
        else
            makeAppointment False ns (returnDate (splitOn " " dt)) ans

{-Function to check service-}
                
checkService :: String -> String -> String -> [Specialist] -> IO()
checkService ns dt serv specialists = do
    let listServices = (concat (map getServices (map services specialists)))
    if 
        elem serv listServices 
    then
        if 
            dt == "No" 
        then
            makeAppointment True ns ((asc (getDates (freeDates 
                 (findSpecialist serv specialists)))) !! 0) serv
        else
            if 
                checkDate (splitOn " " dt) (findSpecialist serv specialists) 
            then
                makeAppointment True ns ((asc (getDates (freeDates 
                     (findSpecialist serv specialists)))) !! 0) serv
            else
                makeAppointment False ns (returnDate (splitOn " " dt)) 
                     serv
    else
        --print("Sorry, we do not provide this service.")
        --print("Please, choose a service from the list of offered:")
        chooseService ns dt specialists

{-Function when deleting a request to file out the Person name and surname 
and then check their belonging to the files-}
    
res2 :: [String] -> IO()
res2 [] = sorryNotName
res2 (file:xs) = do
    print("Please, write your name and surname separated by space:")
    nmsrnm <- getLine
    if checkNameSurname (splitOn " " nmsrnm) (lines file) then do
        getTimeDay (lines file)
    else do
        if xs == [] then do
            sorryNotName
        else do
            res2 xs

{-Function to check time and day whether or not this date is in the file-}
        
getTimeDay :: [String] -> IO()
getTimeDay str_lst_file1 = do
    print("Please, write time and day when you have an appointment in the \
	     \format - 10 Monday:")
    tmdt <- getLine --input of the date of the request the user wants to delete
    if
        checkDateFile (splitOn " " tmdt) str_lst_file1
        {-the checking whether or not this date is right-}
    then do
        deleteRequest (returnDate (splitOn " " tmdt)) (makeBusy str_lst_file1)
    else do
        sorryNotWritten

{-Function to show the user that request was or was not deleted-}

deleteRequest :: (Time, Day) -> Dates -> IO()
deleteRequest (_, _) (Dates []) = do
    print("Error - you do not have an appointment.")
deleteRequest (t, d) (Dates ((t1, d1):xs)) = do 
    if (t1 == t) && (d1 == d) then
        print("Your appointment was succesfully deleted.")
    else
        (deleteRequest (t, d) (Dates xs))

{-Function to form an appointment-}

makeAppointment :: Bool -> String -> (Time,Day) -> String -> IO()
makeAppointment flag nsc (t, d) serv = do
    showAppointment flag (Appointment (Person (lst !! 0) (lst !! 1) (lst !! 2)) 
         t d serv)
        where
            lst = splitOn " " nsc
            {-as function receives a string of user's input 
			program needs to split it-}

{-Function to show formed appointment-}

showAppointment :: Bool -> Appointment -> IO()
showAppointment flag ((Appointment {client = Person {name = n, surname = s, 
     contact = c}, time = t, day = d, service = serv})) = do
    if flag == True then
        print("We wil provide you the nearest available date. \
		     \Your appointment in the process...")
    else
        print("Your appointment in the process...")
    print("New client: " ++ n ++ " " ++ s ++ " " ++ c ++ "; \
	     \you signed up for " ++ show t ++ " " ++ show d ++ " " ++ serv)

{-Functions to print the answers for the trying of deleting the request-}

sorryNotWritten :: IO()
sorryNotWritten = print("Sorry, you do not have an appointment at this time.")

sorryNotName :: IO()
sorryNotName = print("Sorry, there are not your name and surname on the list.")

{-Function to show free time periods for the specialists-}

res3 :: [Specialist] -> IO()
res3 [] = print("Error - there is no information in the files.")
res3 (Specialist {fio = f, busyDates = _, freeDates = fr, services = _}:[]) 
     = print("Our " ++ f ++ " provides services at " ++ show (getDates fr))
res3 (Specialist {fio = f, busyDates = _, freeDates = fr, services = _}:xs) = do
    print("Our " ++ f ++ " provides services at " ++ show (getDates fr))
    res3 xs