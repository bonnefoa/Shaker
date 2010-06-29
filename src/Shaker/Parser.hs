module Shaker.Parser
 where

import Text.ParserCombinators.Parsec
import Shaker.Type

commandParse :: GenParser Char st Command
commandParse = typeDuration >>= \dur ->
  typeAction >>= \act ->
  return (Command dur act)

typeAction :: GenParser Char st Action
typeAction =  skipMany (char ' ') >>
  choice [loadParser,compileParser, quickCheckParser]

loadParser = string "Load" >> return Load
compileParser = string "Compile" >> return Compile
quickCheckParser = string "QuickCheck" >> return QuickCheck

typeDuration :: GenParser Char st Duration
typeDuration = skipMany (char ' ') >>
  option OneShot (char '~' >> return Continuous)

