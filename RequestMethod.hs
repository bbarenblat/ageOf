{- RequestMethod.hs -- how to request somebody's age off the Internet
Copyright (C) 2012  Benjamin Barenblat <benjamin@barenblat.name>

This module is a part of ageOf.

ageOf is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

ageOf is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
ageOf.  If not, see <http://www.gnu.org/licenses/>. -}

module RequestMethod ( RequestMethod(..)
                     , getAge
                     ) where

import Control.Applicative ((<$>))
import Data.Time

data RequestMethod = AgeFunction (String -> IO (Maybe Integer))
                   | BirthDateFunction (String -> IO (Maybe Day))

getAge :: RequestMethod -> String -> IO (Maybe Integer)
getAge (AgeFunction f) name = f name
getAge (BirthDateFunction f) name = do
  today <- localDay <$> zonedTimeToLocalTime <$> getZonedTime
  maybeDate <- f name
  return $ diffYears today <$> maybeDate

diffYears :: Day -> Day -> Integer
new `diffYears` old =
  let (oldYear, _, _) = toGregorian old
      (newYear, _, _) = toGregorian new in
  let yearsDelta = newYear - oldYear in
  if addGregorianYearsRollOver yearsDelta old > new
  then yearsDelta - 1
  else yearsDelta
