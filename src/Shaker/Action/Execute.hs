module Shaker.Action.Execute
 where
  
import Shaker.Type
import Shaker.Reflexivite
import Control.Monad.Reader
import Control.Arrow
import Data.List

runExecute :: Plugin
runExecute = do
  arg <- asks argument
  launchFunction arg

launchFunction :: Maybe String -> Plugin 
launchFunction Nothing = lift $ putStrLn "No action to execute. Give an argument of ModuleName.functionName. The function should be of type IO()"
launchFunction (Just actStr) = runFunction runnableFunction
  where runnableFunction = parseModuleAndAction actStr

parseModuleAndAction :: String -> RunnableFunction
parseModuleAndAction actStr 
  | '>' `elem` actStr = RunnableFunction (split ',' moduleStr)  functionStr
  | otherwise = RunnableFunction [""] actStr
  where (functionStr, moduleStr) =  first reverse . second ( reverse . tail )  . span (/= '>') . reverse $ actStr

split :: Char -> String -> [String]
split sep = takeWhile (not . null) . unfoldr (Just . span (/= sep) . dropWhile (== sep))

