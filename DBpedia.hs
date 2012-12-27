{- DBpedia.hs -- requesting ages from DBpedia
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

module DBpedia (dbpedia) where

import Control.Monad (void)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import qualified Data.Time as Time (Day, parseTime)
import qualified Data.RDF as RDF (Node(..), LValue(..))
import qualified Data.Text as Text (unpack)
import System.CurrentLocale (currentLocale)
import System.Locale (TimeLocale)

import RequestMethod (RequestMethod(BirthDateFunction))

-- The DBpedia plugin, defined by the single function 'getBirthDate'.
dbpedia :: RequestMethod
dbpedia = BirthDateFunction getBirthDate

getBirthDate :: String -> IO (Maybe Time.Day)
getBirthDate name = do
  locale <- currentLocale
  response <- askDBpedia $ selectBirthDate name
  return $ case response of
      Nothing -> Nothing
      Just Unbound -> Nothing
      Just (Bound node) -> parseAsDate locale node

{- Generates a DBpedia SELECT query for a person's birth date.  You can see the
generated query by running 'putStrLn $ createSelectQuery simpleSelect'. -}
selectBirthDate :: String -> Query SelectQuery
selectBirthDate name = do
  -- Variables
  page <- var
  birthDate <- var
  -- Prefixes
  dbpprop <- prefix "dbprop" $ iriRef "http://dbpedia.org/property/"
  rdfs <- prefix "rdfs" $ iriRef "http://www.w3.org/2000/01/rdf-schema#"
  -- Okay, here we go.
  let query = SelectQuery [birthDate]
  void $ triple page (rdfs .:. "label") (name, "en")
  void $ triple page (dbpprop .:. "birthDate") birthDate
  return query

{- Runs a SELECT query for a single variable on DBpedia, returning the first
possible match. -}
askDBpedia :: Query SelectQuery -> IO (Maybe BindingValue)
askDBpedia q = do
  result <- selectQuery "http://dbpedia.org/sparql" q
  return $ case result of
    Just ([x]:_) -> Just x
    Just ((_:_):_) -> error "got more than one bound variable"
    _ -> Nothing

-- Parses an RDF literal as a date.
parseAsDate :: TimeLocale -> RDF.Node -> Maybe Time.Day
parseAsDate locale (RDF.LNode node) =
  let value =
        case node of
          RDF.PlainL v -> v
          RDF.PlainLL v _lang -> v
          RDF.TypedL v _type -> v
  in parseXMLSchemaDate locale $ Text.unpack value
parseAsDate _ _ = Nothing

{- Parses a date somewhat as described in the XML Schema
<http://www.w3.org/TR/xmlschema-2/#date>.  TODO: Handle large (>12-hour) time
zone offsets correctly.  TODO: Handle BCE (negative) years correctly. -}
parseXMLSchemaDate :: TimeLocale -> String -> Maybe Time.Day
parseXMLSchemaDate locale dateString =
  let expectedFormat =
        if length dateString > length "YYYY-MM-DD"
        then "%F%z" -- There's a time zone.
        else "%F"
  in Time.parseTime locale expectedFormat dateString
