{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Parser
Description : Parsing structure and functions for .npp (nginx templates) files
-}
module Nginx.Parser
( NPPProperty
, NPPTree
, NPPValue(..)
, Parsed
, parse
, stripVoid
) where

import safe qualified Data.Char            as Char (isSpace)
import safe           Control.Monad.Except
import safe           Utils                        (prependMB)

-- | Result of "parse" calls that can fail
type Parsed = Except ParseError

data ParseError = SyntaxError    String   [NPPToken]
                | IndentMismatch NPPToken NPPToken
                | GenericError   String

instance Show ParseError where
  show (SyntaxError    e s) = "Syntax error: " ++ show e ++ "\nExpr: " ++ show s
  show (IndentMismatch e g) = "Indentation mismatch.\nExpected: " ++ show e ++ "\nGot: " ++ show g
  show (GenericError   s)   = "Parse error: " ++ s

-- | Fully parsed NPP structure
type NPPTree = [NPPProperty]

-- | NPP Property (key/value pair)
type NPPProperty = (String, NPPValue)

data NPPToken = TString    String
              | TSpace     Char
              | TBlockDecl
              deriving (Eq, Show)

-- | Value of a property
data NPPValue = NPPVoid                            -- ^ No value
              | NPPVal   String                    -- ^ Single string value
              | NPPList  [NPPValue]                -- ^ Multiple strings value
              | NPPBlock (NPPValue, [NPPProperty]) -- ^ Block containing other properties

instance Show NPPValue where
  show NPPVoid           = "Void"
  show (NPPVal x)        = "Value "    ++ show x
  show (NPPList x)       = "Multiple " ++ show x
  show (NPPBlock (x, y)) = "Block "    ++ show x ++ ": " ++ show y

mustParse :: String -> NPPTree
mustParse str = catchError (parse str) (\err -> error err)

-- | Parses a NPP file and returns its parsed structure
parse :: String -> Parsed NPPTree
parse = parseLines . map (tokenize . trimRight) . filter nocomments . filter nonempty . lines

nonempty :: String -> Bool
nonempty str = not (null (dropWhile Char.isSpace str))

nocomments :: String -> Bool
nocomments = (/= '#') . head . dropWhile Char.isSpace

trimRight :: String -> String
trimRight = reverse . dropWhile (\x -> Char.isSpace x || (x == ';')) . reverse

tokenize :: String -> [NPPToken]
tokenize str = tokenize' str ""

tokenize' :: String -> String -> [NPPToken]
tokenize' ""       ""                  = []
tokenize' ""       cw                  = [TString cw]
tokenize' [':']    cw                  = tokenize' "" cw ++ [TBlockDecl]
tokenize' (x  :xs) cw | Char.isSpace x = tokenize' "" cw ++ TSpace x : tokenize' xs ""
                      | otherwise      = tokenize' xs (cw ++ [x])

isIndent :: [NPPToken] -> Bool
isIndent []           = False
isIndent (TSpace _:_) = True
isIndent (_       :_) = False

getIndentation :: [NPPToken] -> [NPPToken]
getIndentation []            = []
getIndentation (TSpace x:xs) = TSpace x : getIndentation xs
getIndentation (_       :_ ) = []

dropIndent :: [NPPToken] -> [NPPToken] -> Parsed [NPPToken]
dropIndent []     y                  = return y
dropIndent (x:xs) (y:ys) | x == y    = dropIndent xs ys
                         | otherwise = throwError $ IndentMismatch x y

trimIndent :: [[NPPToken]] -> Parsed [[NPPToken]]
trimIndent []   = return []
trimIndent list = mapM (dropIndent indent) list
  where
    indent = getIndentation first
    first  = head list

parseValue :: NPPToken -> Parsed NPPValue
parseValue (TSpace  _)  = return NPPVoid
parseValue (TString x)  = return $ NPPVal x
parseValue x            = throwError $ SyntaxError "Invalid token" [x]

-- | Strip NPPVoid values from NPPValue list
stripVoid :: [NPPValue] -> [NPPValue]
stripVoid []           = []
stripVoid (NPPVoid:xs) = stripVoid xs
stripVoid (x      :xs) = x : stripVoid xs

wrapValue :: [NPPValue] -> NPPValue
wrapValue []     = NPPVoid
wrapValue [x]    = x
wrapValue xs     = NPPList xs

parseLines :: [[NPPToken]] -> Parsed [NPPProperty]
parseLines []                     = return []
parseLines (x:xs) | isBlockDecl x = trimIndent indented
                                      >>= \block -> prependMB (parseBlockExpr x block) (parseLines rest)
                    | otherwise     = prependMB (parseProperty x) (parseLines xs)
                    where
                      (indented, rest) = span isIndent xs

isBlockDecl :: [NPPToken] -> Bool
isBlockDecl = elem TBlockDecl

parseProperty :: [NPPToken] -> Parsed NPPProperty
parseProperty (TString str : rest) = mapM parseValue rest
                                       >>= \vals -> return (str, wrapValue $ stripVoid vals)
parseProperty x                    = throwError $ SyntaxError "Unrecognized property type" x

parseBlockExpr :: [NPPToken] -> [[NPPToken]] -> Parsed NPPProperty
parseBlockExpr x block =
  parseLines block
    >>= \parsedblock -> parseProperty (takeWhile (/= TBlockDecl) x)
    >>= \prop        -> return (fst prop, NPPBlock (snd prop, parsedblock))