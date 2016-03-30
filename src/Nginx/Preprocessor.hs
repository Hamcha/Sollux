{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Preprocessor
Description : Processes .npp structures to fill in macros, functions etc
-}
module Nginx.Preprocessor
( PPContext
, PPValue(..)
, process
) where

import safe           Nginx.Parser                 (NPPProperty, NPPValue(..), parse)
import safe qualified Data.List        as List     (intercalate, unionBy, isPrefixOf, isSuffixOf)
import safe qualified System.Directory as Dir      (listDirectory)
import safe qualified System.FilePath  as Filepath (combine, dropFileName, splitFileName, hasExtension)
import safe qualified System.IO        as IO       (readFile)
import safe           Utils                        (must)

type PPContext  = [PPProperty]

type PPProperty = (String, PPValue)

type PPFunction = [NPPValue] -> IO [NPPProperty]

data PPValue = PPVar  String
             | PPList [PPValue]
             | PPFunc PPFunction

instance Show PPValue where
  show (PPVar  x) = x
  show (PPList x) = "List<" ++ ((List.intercalate "," . map show) x) ++ ">"
  show (PPFunc _) = "Function()"

instance Eq PPValue where
  (==) (PPVar  x) (PPVar  y) = x == y
  (==) _          _          = False

-- | Process an NPP tree (NPP files)
process :: PPContext -> [NPPProperty] -> IO [NPPProperty]
process _ []                         = -- No instructions left
  return []
process c (('@':x, y):xs)            = -- Preprocessor instruction, handle it
  execute c x y
    >>= \(ctx, prop) -> (++) prop <$> process ctx xs
process c ((x, NPPBlock (val,y)):xs) = -- Recurse processing in blocks
  process c y
    >>= \py -> (++) [(x, NPPBlock (processPlain c val, py))] <$> process c xs
process c ((x, val):xs)              = -- Plain property, process its values
  (++) [(x, processPlain c val)] <$> process c xs

-- | Process context from an NPP tree (NPP libs)
processLib :: PPContext -> [NPPProperty] -> IO PPContext
processLib c []              = -- No instructions left
  return $ filter isExported c
  where
    isExported x       = elem (PPVar $ fst x) exported
    exported           = extract . must $ lookup "exported" c
    extract (PPList x) = x
    extract _          = error "\"exported\" is not a list (?)"
processLib c (('@':x, y):xs) = -- Strip @ (not needed in libs)
  processLib c ((x,y):xs)
processLib c ((    x, y):xs) = -- Parse and execute instruction
  execute c x y
    >>= \(ctx, _) -> processLib ctx xs

execute :: PPContext -> String -> NPPValue -> IO (PPContext, [NPPProperty])
-- "set KEY VAL"
--  Add <KEY> to the context with value <VAL>
execute ctx "set" (NPPList ((NPPVal key):(NPPVal val):_)) =
  return (newctx, [])
  where
    newctx = updateContext ctx [(key, PPVar val)]
-- "func NAME [ARGS]: CONTENT"
--  Declare a function called NAME and with arguments [ARGS] (separated by space)
execute ctx "func" (NPPBlock (NPPList (NPPVal name:nargs), content)) =
  return (newctx, [])
  where
    newctx  = updateContext ctx [(name, PPFunc fn)]
    fn args = process fnctx content
              where
                fnctx  = updateContext ctx argctx
                argctx = zip (map npptostr nargs) (map npptopp args)
    npptopp  (NPPVal v) = PPVar v
    npptopp  x          = error $ "Non-Val PP Variable: " ++ (show x)
    npptostr (NPPVal v) = v
    npptostr x          = error $ "Non-Var NPP Value: " ++ (show x)
-- "include PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP files
--  and append their trees to the current one
execute ctx "include" (NPPVal y) =
  getFilenames filename
    >>= sequence . map (includeFile ctx)
    >>= \out -> return (ctx, concat out)
  where
    filename = includepathdiff ctx ".npp" y
-- "uselib PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP libraries
--  and append their contexts to the current one
execute ctx "uselib" (NPPVal y) =
  getFilenames filename
    >>= sequence . map (includeLib ctx)
    >>= \out -> return (foldr updateContext ctx out, [])
  where
    filename = includepathdiff ctx ".npplib" y
-- "export VAR"
--  Add <VAR> to the export table so it gets exposed to the parent file
execute ctx "export" (NPPVal y) =
  return (newctx, [])
  where
    newctx = updateContext ctx [("exported", append curexp y)]
    curexp = must $ lookup "exported" ctx
    append (PPList x) n = PPList $ (PPVar n):x
    append x          _ = error $ "Called append on " ++ show x
-- Check for function calls
execute ctx name args =
  case lookup name ctx of
    Just (PPFunc fn) ->
      fn (unwrapValue $ processPlain ctx args)
        >>= \result -> return (ctx, result)
    Just (PPVar _) ->
      error $ "Called NPP val \"" ++ name ++ "\" with params: " ++ (show args) ++ "\n but it is a variable."
    Just (PPList _) ->
      error $ "Called NPP val \"" ++ name ++ "\" with params: " ++ (show args) ++ "\n but it is a list."
    Nothing ->
      error $ "Unknown NPP directive \"" ++ name ++ "\" with params: " ++ (show args) ++ "\n\nContext: " ++ (show ctx)

getFilenames :: FilePath -> IO [FilePath]
getFilenames x | elem '*' x = lsMatch base fleft right
               | otherwise  = return [x]
               where
                 (base, fleft) = Filepath.splitFileName left
                 (left, right) = dropSep $ span (/= '*') x

lsMatch :: FilePath -> FilePath -> FilePath -> IO [FilePath]
lsMatch base left right =
  Dir.listDirectory base
    >>= return . map fullpath . filter match
  where
    fullpath x = base ++ x
    match x = (List.isPrefixOf left x) && (List.isSuffixOf right x)

includeFile :: PPContext -> FilePath -> IO [NPPProperty]
includeFile ctx filename =
  IO.readFile filename
  >>= process newctx . parse
  where
    newctx = updateContext ctx [("filepath", PPVar filename)]

includeLib :: PPContext -> FilePath -> IO PPContext
includeLib ctx filename =
  IO.readFile filename
  >>= processLib newctx . parse
  where
    newctx = updateContext ctx [("filepath", PPVar filename), ("exported", PPList [])]

processPlain :: PPContext -> NPPValue -> NPPValue
processPlain _ (NPPVoid  ) = NPPVoid
processPlain c (NPPVal  x) = NPPVal  $ processValue c x
processPlain c (NPPList x) = NPPList $ map (processPlain c) x
processPlain _ x           = error $ "Unknown type of NPPValue passed to processPlain: " ++ show x

processValue :: PPContext -> String -> String
processValue _ []       = []
processValue c ('@':xs) = value ++ processValue c rest
                          where
                            value       = show . must $ lookup var c
                            (var, rest) = extractVariableName xs
processValue c (x  :xs) = x : processValue c xs

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
  Filepath.combine cwd . fixExtension ext
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