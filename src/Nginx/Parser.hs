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
, mustParse
, parse
, stripVoid
) where

import safe qualified Data.Char            as Char (isSpace)
import safe           Control.Monad.Except
import safe           Utils                        (prependMB)

-- | Result of "parse" calls that can fail
type Parsed = Either (Int, ParseError)

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

type NPPLine = (Int, [NPPToken])

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

-- | "parse" wrapper that hard fails on parse errors
mustParse :: String -> String -> NPPTree
mustParse fname str = case (parse str) of
    (Left  err ) -> error $ formatParseError fname err
    (Right tree) -> tree

formatParseError :: String -> (Int, ParseError) -> String
formatParseError fname (line, err) =
  "Parse error while reading " ++ fname ++ "\nLine: " ++ (show line) ++ "\n" ++ (show err)

-- | Parses a NPP file and returns its parsed structure
parse :: String -> Parsed NPPTree
parse = parseLines . nmap (tokenize . trimRight) . nfilter nocomments . nfilter nonempty . nlines

nlines :: String -> [(Int, String)]
nlines str = zip [1..length strlines] strlines where strlines = lines str

nfilter :: (a -> Bool) -> [(b, a)] -> [(b, a)]
nfilter fn = filter customfn where customfn (_, x) = fn x

nmap :: (a -> b) -> [(c, a)] -> [(c, b)]
nmap fn = map (napply fn)

nspan :: (a -> Bool) -> [(c, a)] -> ([(c, a)], [(c, a)])
nspan fn = span customfn where customfn (_, x) = fn x

napply :: (a -> b) -> (c, a) -> (c, b)
napply fn (n, a) = (n, fn a)

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

dropIndent :: [NPPToken] -> NPPLine -> Parsed NPPLine
dropIndent []     y                       = return y
dropIndent (x:xs) (n, (y:ys)) | x == y    = dropIndent xs (n, ys)
                              | otherwise = throwError (n, IndentMismatch x y)

trimIndent :: [NPPLine] -> Parsed [NPPLine]
trimIndent []   = return []
trimIndent list = mapM (dropIndent indent) list
  where
    indent = getIndentation first
    first  = snd $ head list

parseValue :: Int -> NPPToken -> Parsed NPPValue
parseValue _ (TSpace  _)  = return NPPVoid
parseValue _ (TString x)  = return $ NPPVal x
parseValue n x            = throwError (n, SyntaxError "Invalid token" [x])

-- | Strip NPPVoid values from NPPValue list
stripVoid :: [NPPValue] -> [NPPValue]
stripVoid []           = []
stripVoid (NPPVoid:xs) = stripVoid xs
stripVoid (x      :xs) = x : stripVoid xs

wrapValue :: [NPPValue] -> NPPValue
wrapValue []     = NPPVoid
wrapValue [x]    = x
wrapValue xs     = NPPList xs

parseLines :: [NPPLine] -> Parsed [NPPProperty]
parseLines []                          = return []
parseLines ((n, x):xs) | isBlockDecl x = trimIndent indented
                                           >>= \block -> prependMB (parseBlockExpr (n, x) block) (parseLines rest)
                       | otherwise     = prependMB (parseProperty (n, x)) (parseLines xs)
                       where
                         (indented, rest) = nspan isIndent xs

isBlockDecl :: [NPPToken] -> Bool
isBlockDecl = elem TBlockDecl

parseProperty :: NPPLine -> Parsed NPPProperty
parseProperty (n, (TString str : rest)) = mapM (parseValue n) rest
                                            >>= \vals -> return (str, wrapValue $ stripVoid vals)
parseProperty (n, x)                    = throwError (n, SyntaxError "Unrecognized property type" x)

parseBlockExpr :: NPPLine -> [NPPLine] -> Parsed NPPProperty
parseBlockExpr (n, x) block =
  parseLines block
    >>= \parsedblock -> parseProperty (n, (takeWhile (/= TBlockDecl) x))
    >>= \prop        -> return (fst prop, NPPBlock (snd prop, parsedblock))