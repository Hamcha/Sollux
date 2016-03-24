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
process ctx = fmap concat . sequence . map (processLine ctx)

processLine :: PPContext -> NPPProperty -> IO [NPPProperty]
processLine c (('@':x), y)          = execute c x y -- Preprocessor instruction, handle it
processLine c (x, NPPBlock (val,y)) =               -- Recurse processing in blocks
  process c y >>= \py -> return [(x, NPPBlock (val, py))]
processLine _ x                     = return [x]    -- Plain property, leave it as it is

execute :: PPContext -> String -> NPPValue -> IO [NPPProperty]
execute c "include" (NPPVal y) = do
  let fname = includepathdiff c y
  file <- IO.readFile fname
  let nc = updateContext c [("filepath", fname)]
  process nc $ parse file
execute _ x         y          = error $ "Unknown NPP directive \"" ++ x ++ "\" with params: " ++ (show y)

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