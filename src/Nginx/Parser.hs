{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Parser
Description : Parsing structure and functions for .npp (nginx templates) files
-}
module Nginx.Parser
( LineInfo(..)
, lineFileInfo
, NPPProperty(..)
, NPPTree
, NPPValue(..)
, Parsed
, mustParse
, parse
, stripVoid
) where

import safe           Control.Monad.Except         (throwError)
import safe qualified Data.Char            as Char (isSpace)
import safe           Utils                        (prependMB)

-- | Result of "parse" calls that can fail
type Parsed = Either ParseError

data ParseError = NPPErr LineInfo ParseErrorInfo

data ParseErrorInfo = SyntaxError    String   [NPPToken]
                    | IndentMismatch NPPToken NPPToken
                    | GenericError   String

instance Show ParseErrorInfo where
  show (SyntaxError    e s) = "Syntax error: " ++ show e ++ "\nExpr: " ++ show s
  show (IndentMismatch e g) = "Indentation mismatch.\nExpected: " ++ show e ++ "\nGot: " ++ show g
  show (GenericError   s)   = "Parse error: " ++ s

-- | Line info (for errors)
data LineInfo = LineInfo { fileName   :: String
                         , lineNumber :: Int
                         , lineString :: String
                         } deriving Eq

lineFileInfo :: LineInfo -> String
lineFileInfo l = fileName l ++ ":" ++ (show . lineNumber) l

-- | Fully parsed NPP structure
type NPPTree = [NPPProperty]

-- | NPP Property (key/value pair)
data NPPProperty = NPPProp { lineInfo  :: LineInfo
                           , propKey   :: String
                           , propValue :: NPPValue
                           } deriving Eq

instance Show NPPProperty where
  show (NPPProp line key value) = "[" ++ lineFileInfo line ++ "] " ++ key ++ ": " ++ show value

type NPPLine = (LineInfo, [NPPToken])

data NPPToken = TString    String
              | TSpace     Char
              | TBlockDecl
              deriving (Eq, Show)

-- | Value of a property
data NPPValue = NPPVoid                            -- ^ No value
              | NPPVal   String                    -- ^ Single string value
              | NPPList  [NPPValue]                -- ^ Multiple strings value
              | NPPBlock (NPPValue, [NPPProperty]) -- ^ Block containing other properties
              deriving Eq

instance Show NPPValue where
  show NPPVoid           = "Void"
  show (NPPVal x)        = "Value "    ++ show x
  show (NPPList x)       = "Multiple " ++ show x
  show (NPPBlock (x, y)) = "Block "    ++ show x ++ ": " ++ show y

-- | "parse" wrapper that hard fails on parse errors
mustParse :: String -> String -> NPPTree
mustParse fname str = case parse fname str of Left  err  -> error $ formatParseError err
                                              Right tree -> tree

-- | Render error details to string (to be printed)
formatParseError :: ParseError -> String
formatParseError (NPPErr line err) =
  "Parse error:\n" ++ lineFileInfo line ++ ": " ++ lineString line ++ "\n  " ++ show err

-- | Parses a NPP file and returns its parsed structure
parse :: String -> String -> Parsed NPPTree
parse fname = parseLines . nmap (tokenize . trimRight) . nfilter nocomments . nfilter nonempty . nlines fname

-- | Attach line metadata (filename, number, ..) to each line to parse
nlines :: String -> String -> [(LineInfo, String)]
nlines fname str = zip linfo strlines
  where
    linfo = map (\(x, y) -> LineInfo fname x $ trimSpace y) $ zip [1..length strlines] strlines
    strlines = lines str

-- | Filter wrapper for NPPLine
nfilter :: (a -> Bool) -> [(b, a)] -> [(b, a)]
nfilter fn = filter customfn where customfn (_, x) = fn x

-- | Map wrapper for NPPLine
nmap :: (a -> b) -> [(c, a)] -> [(c, b)]
nmap fn = map (napply fn)

-- | Span wrapper for NPPLine
nspan :: (a -> Bool) -> [(c, a)] -> ([(c, a)], [(c, a)])
nspan fn = span customfn where customfn (_, x) = fn x

-- | Function application wrapper for NPPLine
napply :: (a -> b) -> (c, a) -> (c, b)
napply fn (n, a) = (n, fn a)

-- | Check if a line is empty after trimming whitespace
nonempty :: String -> Bool
nonempty str = not (null (dropWhile Char.isSpace str))

-- | Check if a line is not a comment (begins with #) after trimming whitespace
nocomments :: String -> Bool
nocomments = (/= '#') . head . dropWhile Char.isSpace

-- | Trim right whitespace
trimRight :: String -> String
trimRight = reverse . trimSpace . reverse

-- | Trim left whitespace
trimSpace :: String -> String
trimSpace = dropWhile (\x -> Char.isSpace x || (x == ';'))

-- | Exposed tokenizing function (calls internal tokenize')
tokenize :: String -> [NPPToken]
tokenize str = tokenize' str ""

-- | Internal tokenizer function
tokenize' :: String -> String -> [NPPToken]
tokenize' ""       ""                  = []
tokenize' ""       cw                  = [TString cw]
tokenize' [':']    cw                  = tokenize' "" cw ++ [TBlockDecl]
tokenize' (x  :xs) cw | Char.isSpace x = tokenize' "" cw ++ TSpace x : tokenize' xs ""
                      | otherwise      = tokenize' xs (cw ++ [x])

-- | Check if token list has indentation
isIndent :: [NPPToken] -> Bool
isIndent []           = False
isIndent (TSpace _:_) = True
isIndent (_       :_) = False

-- | Get indentation from token list
getIndentation :: [NPPToken] -> [NPPToken]
getIndentation []            = []
getIndentation (TSpace x:xs) = TSpace x : getIndentation xs
getIndentation (_       :_ ) = []

-- | Exposed function to trim indentation from token list (calls internal "dropIndent")
trimIndent :: [NPPLine] -> Parsed [NPPLine]
trimIndent []   = return []
trimIndent list = mapM (dropIndent indent) list
  where
    indent = getIndentation first
    first  = snd $ head list

-- | Internal function to trim indentation from token list
dropIndent :: [NPPToken] -> NPPLine -> Parsed NPPLine
dropIndent []     y                     = return y
dropIndent (x:xs) (n, y:ys) | x == y    = dropIndent xs (n, ys)
                            | otherwise = throwError $ NPPErr n $ IndentMismatch x y

parseValue :: LineInfo -> NPPToken -> Parsed NPPValue
parseValue _ (TSpace  _)  = return NPPVoid
parseValue _ (TString x)  = return $ NPPVal x
parseValue n x            = throwError $ NPPErr n $ SyntaxError "Invalid token" [x]

-- | Strip NPPVoid values from NPPValue list
stripVoid :: [NPPValue] -> [NPPValue]
stripVoid []           = []
stripVoid (NPPVoid:xs) = stripVoid xs
stripVoid (x      :xs) = x : stripVoid xs

-- Wrap lists into single NPPValue
wrapValue :: [NPPValue] -> NPPValue
wrapValue []     = NPPVoid
wrapValue [x]    = x
wrapValue xs     = NPPList xs

parseLines :: [NPPLine] -> Parsed [NPPProperty]
parseLines []                                = return []
parseLines ((n, x):xs) | TBlockDecl `elem` x = trimIndent indented
                                           >>= \block -> prependMB (parseBlockExpr (n, x) block) (parseLines rest)
                       | otherwise           = prependMB (parseProperty (n, x)) (parseLines xs)
                       where
                         (indented, rest) = nspan isIndent xs

parseProperty :: NPPLine -> Parsed NPPProperty
parseProperty (n, TString str : rest) = mapM (parseValue n) rest
                                          >>= \vals -> return $ NPPProp n str (wrapValue $ stripVoid vals)
parseProperty (n, x)                  = throwError $ NPPErr n $ SyntaxError "Unrecognized property type" x

parseBlockExpr :: NPPLine -> [NPPLine] -> Parsed NPPProperty
parseBlockExpr (n, x) block =
  parseLines block
    >>= \parsedblock -> parseProperty (n, takeWhile (/= TBlockDecl) x)
    >>= \prop        -> return $ NPPProp n (propKey prop) (NPPBlock (propValue prop, parsedblock))