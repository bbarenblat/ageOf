{- SamplePlugin.hs -- a sample request plugin
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

module SamplePlugin (samplePlugin) where

import Data.Time (Day, fromGregorian)

import RequestMethod (RequestMethod(BirthDateFunction))

-- The sample plugin, defined by the single function 'getBirthDate'.
samplePlugin :: RequestMethod
samplePlugin = BirthDateFunction getBirthDate

getBirthDate :: String -> IO (Maybe Day)
getBirthDate "Simon Peyton Jones" = return $ Just $ fromGregorian 1958 1 18
getBirthDate _ = return Nothing
