module Types
    ( Person(..),{- Request(..),-} Appointment(..), Specialist(..), 
         Day(..), Time(..), Services(..), Dates(..)
    ) where

import Data.Monoid() --to connect mappend method to Services and Dates
import Data.Semigroup()

data Person = Person { --the type of the new client
    name :: String,
    surname :: String,
    contact :: String} deriving (Read, Show)

data Appointment = Appointment{ --the type of the final user's appointment
    client :: Person,
    time :: Time,
    day :: Day,
    service :: String} deriving (Read, Show)

data Specialist = Specialist{ --the specialists from files
    fio :: String,
    busyDates :: Dates,
    freeDates :: Dates,
    services :: Services} deriving (Eq, Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday 
     deriving (Read, Show, Ord, Eq) --the type of week days
data Time = AM9 | AM10 | AM11 | PM12 | PM13 | PM14 | PM15 | PM16 | PM17 
     deriving (Read, Show, Ord, Eq) --the type of possible time periods

newtype Services = Services {getServices :: [String]} deriving (Eq, Show)
instance Semigroup Services where
    (<>) (Services x) (Services y) = Services (x ++ y)
instance Monoid Services where
    mempty = Services []
    mappend = (<>) --connecting for the using of combining the lists of newtypes

newtype Dates = Dates {getDates :: [(Time,Day)]} deriving(Eq, Show)
instance Semigroup Dates where
    (<>) (Dates x) (Dates y) = Dates (x ++ y)
instance Monoid Dates where
    mempty = Dates []
    mappend = (<>)
--newtype Name = String
--newtype Surname = String
--newtype Contact = String
--newtype FIO = String