-- | Module responsible to parse a String into a Command
module Shaker.Parser(
  parseCommand
)
 where

import Data.Char

import Text.ParserCombinators.Parsec
import Shaker.Type
import qualified Data.Map as M

-- | Parse the given string to a Command
parseCommand :: String -> ShakerInput -> Either ParseError Command
parseCommand str shIn = parse (typeCommand cmd_map) "parseCommand" str
  where cmd_map = commandMap shIn

-- | Parse a Command
typeCommand :: CommandMap -> GenParser Char st Command
typeCommand cmMap = choice [try typeEmpty, typeCommandNonEmpty cmMap] 

typeCommandNonEmpty :: CommandMap -> GenParser Char st Command 
typeCommandNonEmpty cmMap = typeDuration >>= \dur ->
  typeMultipleAction cmMap >>= \acts ->
  return (Command dur acts)

typeEmpty :: GenParser Char st Command
typeEmpty = spaces >> 
  notFollowedBy anyChar >>
  return emptyCommand

typeMultipleAction :: CommandMap -> GenParser Char st [Action]
typeMultipleAction cmMap = many1 (typeAction cmMap)

-- | Parse to an action
typeAction :: CommandMap -> GenParser Char st Action
typeAction cmMap = skipMany (char ' ') >>
  typeShakerAction cmMap >>= \shAct -> 
  optionMaybe (many $ parseArgument cmMap) >>= \arg ->
  skipMany (char ' ') >> 
  case arg of
       Nothing -> return $ Action shAct
       Just [] -> return $ Action shAct 
       Just list -> return $ ActionWithArg shAct list

parseArgument :: CommandMap -> GenParser Char st String
parseArgument cmMap = 
  skipMany (char ' ') >>
  mapM_ notFollowedBy (parseMapAction cmMap) >>  
  many1 (noneOf " \n") >>= \str ->
  skipMany (char ' ') >>
  return str 

-- | Parse a ShakerAction 
typeShakerAction :: CommandMap -> GenParser Char st ShakerAction
typeShakerAction cmMap = skipMany (char ' ') >>
  choice (parseMapAction cmMap)  >>= \res ->
  notFollowedBy (noneOf " \n") >> 
  skipMany (char ' ') >> return res

parseMapAction :: CommandMap -> [GenParser Char st ShakerAction]
parseMapAction cmMap = map check_key key_list
  where key_list = M.toList cmMap
        check_key (key,value) = try (walk key >> notFollowedBy (noneOf " \n" ) ) >> return value

walk :: String -> GenParser Char st ()
walk [] = return ()
walk (x:xs) = caseChar x >> walk xs
  where caseChar c | isAlpha c = char (toLower c) <|> char (toUpper c)
                   | otherwise = char c

-- | Parse the continuous tag (~)
typeDuration :: GenParser Char st Duration
typeDuration = skipMany (char ' ') >>
  option OneShot (char '~' >> return Continuous)

