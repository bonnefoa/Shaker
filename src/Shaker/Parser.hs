module Shaker.Parser
 where

import Text.ParserCombinators.Parsec
import Shaker.Type

parseCommand :: String -> Command
parseCommand str = case (parse typeCommand "parseCommand" str) of
    Left err -> Command OneShot Help
    Right val -> val

typeCommand :: GenParser Char st Command
typeCommand = typeDuration >>= \dur ->
  typeAction >>= \act ->
  return (Command dur act)

typeAction :: GenParser Char st Action
typeAction =  skipMany (char ' ') >>
  choice [loadParser,compileParser,quitParser, quickCheckParser, helpParser]

typeDuration :: GenParser Char st Duration
typeDuration = skipMany (char ' ') >>
  option OneShot (char '~' >> return Continuous)

loadParser = string "Load" >> return Load
compileParser = string "Compile" >> return Compile
quickCheckParser = try ( string "QuickCheck") >> return QuickCheck
helpParser = string "Help" >> return Help
quitParser = try (string "Quit") >> return Quit


