{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Preprocessor
Description : Processes .npp structures to fill in macros, functions etc
-}
module Nginx.Preprocessor
( PPContext
, PPValue(..)
, mustProcess
, process
) where

import safe           Control.Exception            (try, IOException)
import safe           Control.Monad.Except         (ExceptT, liftIO, runExceptT, throwError)
import safe           Nginx.Parser                 (NPPProperty(..), NPPValue(..), LineInfo(..), lineFileInfo, mustParse)
import safe qualified Data.List        as List     (intercalate, unionBy, isPrefixOf, isSuffixOf)
import safe qualified System.Directory as Dir      (listDirectory)
import safe qualified System.FilePath  as Filepath (dropFileName, splitFileName, hasExtension)
import safe qualified System.IO        as IO       (readFile)
import safe           System.IO.Error              (ioeGetFileName, ioeGetErrorString)
import safe           Utils                        (abspath, must)


data ProcessErrorInfo = UnknownDirective   String
                      | ValNotAFunction    String
                      | ListNotAFunction   String
                      | BoolNotAFunction   String
                      | IncludeFileError   IOException
                      | MalformedCondition ConditionError
                      | MissingIfCondition

instance Show ProcessErrorInfo where
  show (UnknownDirective   s) = "Unknown NPP directive: " ++ s
  show (ValNotAFunction    s) = "Calling \"" ++ s ++ "\" (value) as function"
  show (ListNotAFunction   s) = "Calling \"" ++ s ++ "\" (list) as function"
  show (BoolNotAFunction   s) = "Calling \"" ++ s ++ "\" (bool) as function"
  show (IncludeFileError   e) = "Cannot include file \"" ++ (must . ioeGetFileName) e ++ "\": " ++ ioeGetErrorString e
  show (MalformedCondition c) = "Malformed condition: " ++ (show c)
  show MissingIfCondition     = "Calling 'else'/'elif' with no 'if'"

data ProcessError = PPErr LineInfo ProcessErrorInfo

-- | Result of "process" calls that can fail
type Processed = ExceptT ProcessError IO

type PPContext  = [PPProperty]

type PPProperty = (String, PPValue)

type PPFunction = [NPPValue] -> Processed [NPPProperty]

data PPValue = PPVar  String
             | PPList [PPValue]
             | PPFunc PPFunction
             | PPBool Bool

instance Show PPValue where
  show (PPVar  x) = x
  show (PPList x) = "List<" ++ (List.intercalate "," . map show) x ++ ">"
  show (PPFunc _) = "Function()"
  show (PPBool b) = "Bool<" ++ show b ++ ">"

instance Eq PPValue where
  (==) (PPVar  x) (PPVar  y) = x == y
  (==) _          _          = False

-- | "process" wrapper that hard fails on processing errors
mustProcess :: PPContext -> [NPPProperty] -> IO [NPPProperty]
mustProcess c p = runExceptT (process c p)
    >>= \case Left  err   -> error $ formatProcessError err
              Right props -> return props

formatProcessError :: ProcessError -> String
formatProcessError (PPErr line err) =
  "Processing error:\n" ++ lineFileInfo line ++ ": " ++ lineString line ++ "\n  " ++ show err

-- | Process an NPP tree (NPP files)
process :: PPContext -> [NPPProperty] -> Processed [NPPProperty]
process _ []                                  = -- No instructions left
  return []
process c (NPPProp l ('\\':'@':x) val:xs)     = -- Escaped @, unescape and process
  (++) [NPPProp l ('@':x) (processPlain c val)] <$> process c xs
process c (NPPProp l ('@':x) y:xs)            = -- Preprocessor instruction, handle it
  execute c l x y
    >>= \(ctx, prop) -> (++) prop <$> process ctx xs
process c (NPPProp l x (NPPBlock (val,y)):xs) = -- Recurse processing in blocks
  process c y
    >>= \py -> (++) [NPPProp l x (NPPBlock (processPlain c val, py))] <$> process c xs
process c (NPPProp l x val:xs)              = -- Plain property, process its values
  (++) [NPPProp l x (processPlain c val)] <$> process c xs

-- | Process context from an NPP tree (NPP libs)
processLib :: PPContext -> [NPPProperty] -> Processed PPContext
processLib c []              = -- No instructions left
  return $ filter isExported c
  where
    isExported x       = (PPVar $ fst x) `elem` exported
    exported           = extract . must $ lookup "exported" c
    extract (PPList x) = x
    extract _          = error "\"exported\" is not a list (?)"
processLib c (NPPProp l ('@':x) y:xs) = -- Strip @ (not needed in libs)
  processLib c (NPPProp l x y:xs)
processLib c (NPPProp l x       y:xs) = -- Parse and execute instruction
  execute c l x y
    >>= \(ctx, _) -> processLib ctx xs

execute :: PPContext -> LineInfo -> String -> NPPValue -> Processed (PPContext, [NPPProperty])

-- "set KEY VAL"
--  Add <KEY> to the context with value <VAL>
execute ctx _ "set" (NPPList (NPPVal key:NPPVal val:_)) =
  return (newctx, [])
  where
    newctx = updateContext ctx [(key, PPVar val)]

-- "func NAME [ARGS]: CONTENT"
--  Declare a function called NAME and with arguments [ARGS] (separated by space)
execute ctx l "func" (NPPBlock (NPPVal name, content)) =
  execute ctx l "func" (NPPBlock (NPPList [NPPVal name], content))
execute ctx _ "func" (NPPBlock (NPPList (NPPVal name:nargs), content)) =
  return (newctx, [])
  where
    newctx  = updateContext ctx [(name, PPFunc fn)]
    fn args = process fnctx content
              where
                fnctx  = updateContext ctx argctx
                argctx = zip (map npptostr nargs) (map npptopp args)
    npptopp  (NPPVal v) = PPVar v
    npptopp  x          = error $ "Non-Val PP Variable: " ++ show x
    npptostr (NPPVal v) = v
    npptostr x          = error $ "Non-Var NPP Value: " ++ show x

-- "if COND: CONTENT"
-- Evaluate a condition and append content if condition is met
execute ctx l "if" (NPPBlock (NPPList condition, content)) =
  case runCondition ctx condition of
    Left  err    -> throwError $ PPErr l $ MalformedCondition err
    Right result -> return (newctx, iftrue result content)
                    where
                      iftrue True  a = a
                      iftrue False _ = []
                      newctx = updateContext ctx [("lastConditionResult", PPBool result)]

-- "else: CONTENT"
-- Append content if the last condition was not met
execute ctx l "else" (NPPBlock (_, content)) =
  lastCondition ctx l >>=
    \result -> return (newctx, iffalse result content)
  where
    iffalse False  a = a
    iffalse True   _ = []
    newctx = dropKeyFromContext ctx "lastConditionResult"

-- "elif COND: CONTENT"
-- If the last condition was not met, evaluate an additional condition and append content if that one is met
execute ctx l "elif" v =
  lastCondition ctx l >>= \case True  -> return (ctx, [])
                                False -> execute ctx l "if" v

-- "include PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP files
--  and append their trees to the current one
execute ctx l "include" (NPPVal y) =
  getFilenames filename
    >>= mapM (includeFile ctx l)
    >>= \out -> return (ctx, concat out)
  where
    filename = includepathdiff ctx ".npp" y

-- "uselib PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP libraries
--  and append their contexts to the current one
execute ctx l "uselib" (NPPVal y) =
  getFilenames filename
    >>= mapM (includeLib ctx l)
    >>= \out -> return (foldr updateContext ctx out, [])
  where
    filename = includepathdiff ctx ".npplib" y

-- "export VAR"
--  Add <VAR> to the export table so it gets exposed to the parent file
execute ctx _ "export" (NPPVal y) =
  return (newctx, [])
  where
    newctx = updateContext ctx [("exported", append curexp y)]
    curexp = must $ lookup "exported" ctx
    append (PPList x) n = PPList $ PPVar n : x
    append x          _ = error $ "Called append on " ++ show x

-- Check for function calls
execute ctx l name args =
  case lookup name ctx of
    Just (PPFunc fn) ->
      fn (unwrapValue $ processPlain ctx args)
        >>= \result -> return (ctx, result)
    Just (PPVar _) ->
      throwError $ PPErr l $ ValNotAFunction name
    Just (PPList _) ->
      throwError $ PPErr l $ ListNotAFunction name
    Just (PPBool _) ->
      throwError $ PPErr l $ BoolNotAFunction name
    Nothing ->
      throwError $ PPErr l $ UnknownDirective name

getFilenames :: FilePath -> Processed [FilePath]
getFilenames x | '*' `elem` x = lsMatch base fleft right
               | otherwise    = return [x]
               where
                 (base, fleft) = Filepath.splitFileName left
                 (left, right) = dropSep $ span (/= '*') x

lsMatch :: FilePath -> FilePath -> FilePath -> Processed [FilePath]
lsMatch base left right =
  liftIO $ fmap (map fullpath . filter match) (Dir.listDirectory base)
  where
    fullpath x = base ++ x
    match x = List.isPrefixOf left x && List.isSuffixOf right x

includeFile :: PPContext -> LineInfo -> FilePath -> Processed [NPPProperty]
includeFile ctx l filename =
  (liftIO . try . IO.readFile) filename
    >>= \case Right r -> (process newctx . mustParse filename) r
              Left  e -> throwError $ PPErr l $ IncludeFileError e
  where
    newctx = updateContext ctx [("filepath", PPVar filename)]

includeLib :: PPContext -> LineInfo -> FilePath -> Processed PPContext
includeLib ctx l filename =
  (liftIO . try . IO.readFile) filename
    >>= \case Right r -> (processLib newctx . mustParse filename) r
              Left  e -> throwError $ PPErr l $ IncludeFileError e
  where
    newctx   = updateContext ctx [("filepath", PPVar filename), ("exported", PPList [])]

processPlain :: PPContext -> NPPValue -> NPPValue
processPlain _  NPPVoid    = NPPVoid
processPlain c (NPPVal  x) = NPPVal  $ processValue c x
processPlain c (NPPList x) = NPPList $ map (processPlain c) x
processPlain _ x           = error $ "Unknown type of NPPValue passed to processPlain: " ++ show x

processValue :: PPContext -> String -> String
processValue _ []       = []
processValue c ('\\':'@':xs) = '@' : processValue c xs
processValue c (     '@':xs) = value ++ processValue c rest
                              where
                                value       = show . must $ lookup var c
                                (var, rest) = extractVariableName xs
processValue c (       x:xs) = x : processValue c xs

-- Extract variable name from template string parts
extractVariableName :: String -> (String, String)
extractVariableName []       = ([], [])
extractVariableName ('{':xs) = dropSep $ span (/= '}') xs
extractVariableName x        = (x, [])

-- Drop separator from split operations
dropSep :: (String, String) -> (String, String)
dropSep (a, b) = (a, tail b)

-- Merge values from a new context to an existing one
updateContext :: PPContext -> PPContext -> PPContext
updateContext a b =
  List.unionBy (\x y -> fst x == fst y) b a

-- Delete a key from context
dropKeyFromContext :: PPContext -> String -> PPContext
dropKeyFromContext ctx key = filter (\x -> fst x /= key) ctx

-- Get relative path for current directory from the script's directory
includepathdiff :: PPContext -> String -> FilePath -> FilePath
includepathdiff c ext =
  fixExtension ext . abspath cwd
  where
    cwd  = Filepath.dropFileName base
    base = show . must $ lookup "filepath" c

-- Add file extension to include paths if missing
fixExtension :: String -> FilePath -> FilePath
fixExtension ext fname | not $ Filepath.hasExtension fname = fname ++ ext
                       | otherwise                         = fname

-- Unwrap lists from NPPValue wrappers (or wrap single values into lists)
unwrapValue :: NPPValue -> [NPPValue]
unwrapValue NPPVoid      = []
unwrapValue (NPPList xs) = xs
unwrapValue x            = [x]

-- Check if the last if evaluation failed
lastCondition :: PPContext -> LineInfo -> Processed Bool
lastCondition ctx l = case lookup "lastConditionResult" ctx of
  Nothing         -> throwError $ PPErr l MissingIfCondition
  Just (PPBool x) -> return x
  Just _          -> error $ "lastConditionResult is not a boolean value (??)"

-- | Result of conditional calls that can fail
type Evaluated = Either ConditionError

data ConditionError = WrongParamCount Int Int
                    | InexistantCondition String
                    | InvalidCondition

instance Show ConditionError where
  show (WrongParamCount e g)   = "Not enough or too many parameters (Expected " ++ show e ++ ", found " ++ show g ++ ")"
  show (InexistantCondition s) = "No such condition '" ++ s ++ "'"
  show InvalidCondition        = "Invalid condition"

-- Conditional functions (for if/else)
type ConditionalFn = PPContext -> [NPPValue] -> Evaluated Bool

-- Evaluate condition by parsing and calling sub-functions
runCondition :: ConditionalFn
runCondition ctx ((NPPVal "match"):params) = strmatch ctx params
runCondition _   ((NPPVal x)      :_)      = throwError $ InexistantCondition x
runCondition _   _                         = throwError $ InvalidCondition

-- String match between properties and/or constants
strmatch :: ConditionalFn
strmatch ctx (a:b:[]) = return $ (processPlain ctx a) == (processPlain ctx b)
strmatch _   x        = throwError $ WrongParamCount 2 $ length x