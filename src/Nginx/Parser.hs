{-|
Module      : Nginx.Parser
Description : Parsing structure and functions for .npp (nginx templates) files
-}
module Nginx.Parser
( NPPProperty
, NPPValue(..)
, parse
) where

import qualified Data.Char as Char (isSpace)

-- | NPP Property (key/value pair)
type NPPProperty = (String, NPPValue)

data NPPToken = TString    String
              | TSpace     Char
              | TBlockDecl
              deriving (Eq, Show)

-- | Value of a property
data NPPValue = NPPVoid                -- ^ No value
              | NPPVal   String        -- ^ Single string value
              | NPPList  [NPPValue]    -- ^ Multiple strings value
              | NPPBlock [NPPProperty] -- ^ Block containing other properties

instance Show NPPValue where
  show (NPPVoid)    = "Void"
  show (NPPVal x)   = "Value "  ++ show x
  show (NPPList x)  = "Multiple "  ++ show x
  show (NPPBlock x) = "Block " ++ show x

-- | Parses a NPP file and returns its parsed structure
parse :: String -> [NPPProperty]
parse = parseLines . (map tokenize) . (map trimRight) . (filter nonempty) . lines

nonempty :: String -> Bool
nonempty str = length (dropWhile Char.isSpace str) > 0

trimRight :: String -> String
trimRight = reverse . (dropWhile Char.isSpace) . reverse

tokenize :: String -> [NPPToken]
tokenize str = tokenize' str ""

tokenize' :: String -> String -> [NPPToken]
tokenize' ""     ""                  = []
tokenize' ""     cw                  = [TString cw]
tokenize' (x:xs) cw | Char.isSpace x = (tokenize' "" cw) ++ (TSpace x) : tokenize' xs ""
                    | x == ':'       = (tokenize' "" cw) ++ TBlockDecl : tokenize' xs ""
                    | otherwise      = tokenize' xs (cw ++ [x])

isIndent :: [NPPToken] -> Bool
isIndent (TSpace _:_) = True
isIndent (x:xs)       = False

getIndentation :: [NPPToken] -> [NPPToken]
getIndentation []            = []
getIndentation (TSpace x:xs) = TSpace x : getIndentation xs
getIndentation (_:xs)        = []

dropIndent :: [NPPToken] -> [NPPToken] -> [NPPToken]
dropIndent []     y      = y
dropIndent (x:xs) (y:ys) | x == y    = dropIndent xs ys
                         | otherwise = error $ "Indentation mismatch!\n\nExpected: " ++ (show x) ++ "\nGot: " ++ (show y)

trimIndent :: [[NPPToken]] -> [[NPPToken]]
trimIndent []   = []
trimIndent list = map (dropIndent indent) list
  where
  indent = getIndentation first
  first = head list


parseValue :: NPPToken -> NPPValue
parseValue (TSpace  x)  = NPPVoid
parseValue (TString x)  = NPPVal x
parseValue x            = error $ "Syntax error!\n\nToken: " ++ show x

stripVoid :: [NPPValue] -> [NPPValue]
stripVoid []           = []
stripVoid (NPPVoid:xs) = stripVoid xs
stripVoid (x      :xs) = x : stripVoid xs

wrapValue :: [NPPValue] -> NPPValue
wrapValue []     = NPPVoid
wrapValue (x:[]) = x
wrapValue xs     = NPPList xs

parseLines :: [[NPPToken]] -> [NPPProperty]
parseLines [] = []
parseLines ((TString str:TBlockDecl:_):xs) =
  (str, NPPBlock $ parseLines block) : parseLines rest
  where
  block = trimIndent indented
  (indented, rest) = span isIndent xs
parseLines ((TString str:rest):xs) =
  (str, (wrapValue . stripVoid . map parseValue) rest) : parseLines xs
parseLines x = error $ "Syntax error!\n\nAST: " ++ (show x)