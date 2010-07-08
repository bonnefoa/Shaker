module Shaker.Parser(
  parseCommand
)
 where

import Text.ParserCombinators.Parsec
import Shaker.Type
import qualified Data.Map as M

-- | Parse the given string to a Command
parseCommand :: ShakerInput -> String -> Command
parseCommand shIn str = case (parse (typeCommand $ commandMap shIn) "parseCommand" str) of
    Left err -> Command OneShot Help
    Right val -> val

-- | Parse to a Type
typeCommand :: CommandMap -> GenParser Char st Command
typeCommand cmMap = typeDuration >>= \dur ->
  typeAction cmMap >>= \act ->
  return (Command dur act)

-- | Parse to an action
typeAction :: CommandMap -> GenParser Char st Action
typeAction cmMap =  skipMany (char ' ') >>
  choice (parseMapAction cmMap)

-- | Parse the continuous tag (~)
typeDuration :: GenParser Char st Duration
typeDuration = skipMany (char ' ') >>
  option OneShot (char '~' >> return Continuous)

parseMapAction cmMap= map (\(k,v) -> try (string k) >> return v) (M.toList cmMap)

