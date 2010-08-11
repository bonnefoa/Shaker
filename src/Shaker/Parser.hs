-- | Module responsible to parse a String into a Command
module Shaker.Parser(
  parseCommand
)
 where

import Text.ParserCombinators.Parsec
import Shaker.Type
import qualified Data.Map as M

-- | Parse the given string to a Command
parseCommand :: ShakerInput -> String -> Command
parseCommand shIn str = 
  case (parse (typeCommand $ commandMap shIn) "parseCommand" str) of
    Left _ -> Command OneShot [Action Help] 
    Right val -> val

-- | Parse a Command
typeCommand :: CommandMap -> GenParser Char st Command
typeCommand cmMap = typeDuration >>= \dur ->
  typeMultipleAction cmMap >>= \acts ->
  return (Command dur acts)

typeMultipleAction :: CommandMap -> GenParser Char st [Action]
typeMultipleAction cmMap = many (typeAction cmMap) >>= \res ->
  case res of 
       [] -> return [Action Help]
       _ -> return res

-- | Parse to an action
typeAction :: CommandMap -> GenParser Char st Action
typeAction cmMap = skipMany (char ' ') >>
  typeShakerAction cmMap >>= \shAct -> 
  optionMaybe (parseArgument cmMap) >>= \arg ->
  skipMany (char ' ') >> 
  case arg of
       Nothing -> return $ Action shAct
       Just str -> return $ ActionWithArg shAct str

parseArgument :: CommandMap -> GenParser Char st String
parseArgument cmMap = 
  skipMany (char ' ') >>
  mapM_ (\a -> notFollowedBy $ string (a++" ") ) (M.keys cmMap) >>  
  mapM_ (\a -> notFollowedBy $ string (a++"\n") ) (M.keys cmMap) >>  
  many1 (noneOf " \n") >>= \str ->
  skipMany (char ' ') >>
  return str 

-- | Parse a ShakerAction 
typeShakerAction :: CommandMap -> GenParser Char st ShakerAction
typeShakerAction cmMap = skipMany (char ' ') >>
  choice (parseMapAction cmMap)  >>= \res ->
  notFollowedBy (noneOf " \n") >> 
  skipMany (char ' ') >> return res

-- | Parse the continuous tag (~)
typeDuration :: GenParser Char st Duration
typeDuration = skipMany (char ' ') >>
  option OneShot (char '~' >> return Continuous)

parseMapAction :: CommandMap -> [GenParser Char st ShakerAction]
parseMapAction cmMap = map (\(k,v) -> try (string k) >> return v) (M.toList cmMap)

