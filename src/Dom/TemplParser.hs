module Dom.TemplParser where

-- TODO: figure out backtracking?
--import Prelude hiding (try)

import Text.ParserCombinators.Parsec

import Dom.TemplTypes

data InchoateTempl = Raw String
                   | Eval ID String
                   deriving (Eq, Show)

setID :: ID -> InchoateTempl -> InchoateTempl
setID i (Eval _ x) = Eval i x
setID _ t = t

simplifyTempls :: [InchoateTempl] -> [InchoateTempl]
simplifyTempls [] = []
simplifyTempls (a:b:xs) =
  case (a,b) of
    (Raw s1, Raw s2) -> simplifyTempls ((Raw (s1 ++ s2)) : xs)
    _ -> a : simplifyTempls (b:xs)
simplifyTempls x = x





parseHierarchy :: ID -> String -> Either ParseError [InchoateTempl]
parseHierarchy rootID input = parse (hierarchy rootID) "" input


hierarchy :: ID -> Parser [InchoateTempl]
hierarchy rootID = do
  templs0 <- many (try evalTag <|> rawChar)
  let templs1 = simplifyTempls templs0
  let templs2 = map (\(t,i) -> setID (rootID ++ "-" ++ show i) t) $ zip templs1 ([1..] :: [Integer])
  return templs2



evalTag :: Parser InchoateTempl
evalTag = start >> rest "" >>= (\s -> return $ Eval "" s)
  where start = string "<%="
        end = string "%>"
        rest s = do
          x <- try (end >> return "") <|> (noneOf "" >>= (\x -> rest [x]))
          return (s ++ x)

rawChar :: Parser InchoateTempl
rawChar = do
  c <- noneOf "\0" 
  return $ Raw [c]






