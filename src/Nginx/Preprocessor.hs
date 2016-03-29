{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Preprocessor
Description : Processes .npp structures to fill in macros, functions etc
-}
module Nginx.Preprocessor
( process
) where

import safe           Nginx.Parser                (NPPProperty, NPPValue(..), parse)
import safe qualified Data.List       as List     (unionBy)
import safe qualified System.IO       as IO       (readFile)
import safe qualified System.FilePath as Filepath (combine, dropFileName, hasExtension)
import safe           Utils                       (must)

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
--  Read the contents of <PATH>, parse it as an NPP file and append its tree to the current one
execute ctx "include" (NPPVal y) =
  IO.readFile filename
    >>= process newctx . parse
    >>= \out -> return (ctx, out)
  where
    newctx   = updateContext ctx [("filepath", filename)]
    filename = includepathdiff ctx y
execute _ x y =
  error $ "Unknown NPP directive \"" ++ x ++ "\" with params: " ++ (show y)

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
extractVariableName ('{':xs) = dropLast $ span (/= '}') xs
extractVariableName x        = (x, [])

dropLast :: (String, String) -> (String, String)
dropLast (a, b) = (a, init b)

updateContext :: PPContext -> PPContext -> PPContext
updateContext a b =
  List.unionBy eq b a
  where
    eq (x, _) (y, _) = x == y

includepathdiff :: PPContext -> String -> String
includepathdiff c =
  Filepath.combine cwd . fixExtension
  where
    cwd  = Filepath.dropFileName base
    base = must $ lookup "filepath" c

fixExtension :: String -> String
fixExtension fname | not $ Filepath.hasExtension fname = fname ++ ".npp"
                   | otherwise                         = fname