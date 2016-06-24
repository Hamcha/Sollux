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

import safe           Control.Monad.Except
import safe           Nginx.Parser                 (NPPProperty(..), NPPValue(..), LineInfo(..), mustParse)
import safe qualified Data.List        as List     (intercalate, unionBy, isPrefixOf, isSuffixOf)
import safe qualified System.Directory as Dir      (listDirectory)
import safe qualified System.FilePath  as Filepath (dropFileName, splitFileName, hasExtension)
import safe qualified System.IO        as IO       (readFile)
import safe           Utils                        (abspath, must)


data ProcessErrorInfo = UnknownDirective String
                      | ValNotAFunction  String
                      | ListNotAFunction String

instance Show ProcessErrorInfo where
  show (UnknownDirective s) = "Unknown NPP directive: " ++ s
  show (ValNotAFunction  s) = "Calling \"" ++ s ++ "\" (value) as function"
  show (ListNotAFunction s) = "Calling \"" ++ s ++ "\" (list) as function"

data ProcessError = PPErr LineInfo ProcessErrorInfo

-- | Result of "process" calls that can fail
type Processed = ExceptT ProcessError IO

type PPContext  = [PPProperty]

type PPProperty = (String, PPValue)

type PPFunction = [NPPValue] -> Processed [NPPProperty]

data PPValue = PPVar  String
             | PPList [PPValue]
             | PPFunc PPFunction

instance Show PPValue where
  show (PPVar  x) = x
  show (PPList x) = "List<" ++ (List.intercalate "," . map show) x ++ ">"
  show (PPFunc _) = "Function()"

instance Eq PPValue where
  (==) (PPVar  x) (PPVar  y) = x == y
  (==) _          _          = False

-- | "process" wrapper that hard fails on processing errors
mustProcess :: PPContext -> [NPPProperty] -> IO [NPPProperty]
mustProcess c p = runExceptT (process c p)
    >>= \res -> case res of
      (Left  err  ) -> error $ formatProcessError err
      (Right props) -> return props

formatProcessError :: ProcessError -> String
formatProcessError (PPErr line err) =
  "Processing error in " ++ fileName line ++ ":" ++ (show . lineNumber) line ++ "\n  " ++ show err

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
-- "include PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP files
--  and append their trees to the current one
execute ctx _ "include" (NPPVal y) =
  getFilenames filename
    >>= mapM (includeFile ctx)
    >>= \out -> return (ctx, concat out)
  where
    filename = includepathdiff ctx ".npp" y
-- "uselib PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP libraries
--  and append their contexts to the current one
execute ctx _ "uselib" (NPPVal y) =
  getFilenames filename
    >>= mapM (includeLib ctx)
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

includeFile :: PPContext -> FilePath -> Processed [NPPProperty]
includeFile ctx filename =
  (liftIO . IO.readFile) filename
    >>= process newctx . mustParse filename
  where
    newctx = updateContext ctx [("filepath", PPVar filename)]

includeLib :: PPContext -> FilePath -> Processed PPContext
includeLib ctx filename =
  (liftIO . IO.readFile) filename
    >>= processLib newctx . mustParse filename
  where
    newctx = updateContext ctx [("filepath", PPVar filename), ("exported", PPList [])]

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

extractVariableName :: String -> (String, String)
extractVariableName []       = ([], [])
extractVariableName ('{':xs) = dropSep $ span (/= '}') xs
extractVariableName x        = (x, [])

dropSep :: (String, String) -> (String, String)
dropSep (a, b) = (a, tail b)

updateContext :: PPContext -> PPContext -> PPContext
updateContext a b =
  List.unionBy eq b a
  where
    eq (x, _) (y, _) = x == y

includepathdiff :: PPContext -> String -> FilePath -> FilePath
includepathdiff c ext =
  fixExtension ext . abspath cwd
  where
    cwd  = Filepath.dropFileName base
    base = show . must $ lookup "filepath" c

fixExtension :: String -> FilePath -> FilePath
fixExtension ext fname | not $ Filepath.hasExtension fname = fname ++ ext
                       | otherwise                         = fname

unwrapValue :: NPPValue -> [NPPValue]
unwrapValue NPPVoid      = []
unwrapValue (NPPList xs) = xs
unwrapValue x            = [x]