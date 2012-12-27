{- Main.hs -- main entry point
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

module Main where

import Control.Monad (foldM)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import Text.Printf (printf)

import RequestMethod (RequestMethod, getAge)
import DBpedia (dbpedia)
import SamplePlugin (samplePlugin)

-- The plugins we're going to query, in order of preference.
plugins :: [RequestMethod]
plugins = [ samplePlugin
          , dbpedia ]

main :: IO ()
main = getArgs >>= go >>= exitWith

go :: [String] -> IO ExitCode
go (name : []) = do
  age <- getAgeWith plugins name
  case age of
    Nothing -> return $ ExitFailure 1
    Just n -> print n >> return ExitSuccess
go _ = usage >> return (ExitFailure 1)

{- The workhorse function.  Given a list of 'RequestMethod's, 'getAgeWith'
tries to look up 'name' in each one.  As soon as a match succeeds, it returns
the age. -}
getAgeWith :: [RequestMethod] -> String -> IO (Maybe Integer)
getAgeWith methods name =
  foldM maybeTryNextMethod Nothing methods
  where maybeTryNextMethod :: Maybe Integer -> RequestMethod -> IO (Maybe Integer)
        maybeTryNextMethod v@(Just _) _ = return v
        maybeTryNextMethod Nothing method = getAge method name

usage :: IO ()
usage = do
  programName <- getProgName
  printf "usage: %s <name>\n" programName
