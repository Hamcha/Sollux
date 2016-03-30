{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Preprocessor
Description : Processes .npp structures to fill in macros, functions etc
-}
module Nginx.Preprocessor
( process
) where

import safe           Nginx.Parser                 (NPPProperty, NPPValue(..), parse)
import safe qualified Data.List        as List     (unionBy, isPrefixOf, isSuffixOf)
import safe qualified System.Directory as Dir      (listDirectory)
import safe qualified System.FilePath  as Filepath (combine, dropFileName, splitFileName, hasExtension)
import safe qualified System.IO        as IO       (readFile)
import safe           Utils                        (must)

type PPContext  = [PPProperty]
type PPProperty = (String, String)

-- | Process an NPP tree
process :: PPContext -> [NPPProperty] -> IO [NPPProperty]
process _ []                         = -- No instructions left
  return []
process c (('@':x, y):xs)            = -- Preprocessor instruction, handle it
  execute c x y
    >>= \(ctx, prop) -> (++) prop <$> process ctx xs
process c ((x, NPPBlock (val,y)):xs) = -- Recurse processing in blocks
  process c y
    >>= \py -> (++) [(x, NPPBlock (val, py))] <$> process c xs
process c ((x, val):xs)              = -- Plain property, process its values
  (++) [(x, processPlain c val)] <$> process c xs

execute :: PPContext -> String -> NPPValue -> IO (PPContext, [NPPProperty])
-- "set KEY VAL"
--  Add <KEY> to the context with value <VAL>
execute ctx "set" (NPPList ((NPPVal key):(NPPVal val):_)) =
  return (newctx, [])
  where
    newctx = updateContext ctx [(key, val)]
-- "include PATH"
--  Read the contents of <PATH> (one or more files), parse it as NPP files
--  and append their trees to the current one
execute ctx "include" (NPPVal y) =
  getFilenames filename
    >>= sequence . map (includeFile ctx)
    >>= \out -> return (ctx, concat out)
  where
    filename = includepathdiff ctx y
execute _ x y =
  error $ "Unknown NPP directive \"" ++ x ++ "\" with params: " ++ (show y)

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
    newctx = updateContext ctx [("filepath", filename)]

processPlain :: PPContext -> NPPValue -> NPPValue
processPlain _ (NPPVoid  ) = NPPVoid
processPlain c (NPPVal  x) = NPPVal  $ processValue c x
processPlain c (NPPList x) = NPPList $ map (processPlain c) x
processPlain _ x           = error $ "Unknown type of NPPValue passed to processPlain: " ++ show x

processValue :: PPContext -> String -> String
processValue _ []       = []
processValue c ('@':xs) = value ++ processValue c rest
                          where
                            value       = must $ lookup var c
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

includepathdiff :: PPContext -> FilePath -> FilePath
includepathdiff c =
  Filepath.combine cwd . fixExtension
  where
    cwd  = Filepath.dropFileName base
    base = must $ lookup "filepath" c

fixExtension :: FilePath -> FilePath
fixExtension fname | not $ Filepath.hasExtension fname = fname ++ ".npp"
                   | otherwise                         = fname