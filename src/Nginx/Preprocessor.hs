{-# LANGUAGE Safe #-}

{-|
Module      : Nginx.Preprocessor
Description : Processes .npp structures to fill in macros, functions etc
-}
module Nginx.Preprocessor
( process
) where

import safe           Nginx.Parser       (NPPProperty, NPPValue(..), parse)
import safe qualified System.IO    as IO (readFile)

type PPContext  = [PPProperty]
type PPProperty = (String, String)

-- | Process an NPP tree
process :: PPContext -> [NPPProperty] -> IO [NPPProperty]
process ctx = fmap concat . sequence . map (processLine ctx)

processLine :: PPContext -> NPPProperty -> IO [NPPProperty]
processLine c (('@':x), y)    = execute c x y -- Preprocessor instruction, handle it
processLine c (x, NPPBlock y) = do            -- Recurse processing in blocks
  py <- process c y
  return [(x, NPPBlock py)]
processLine _ x               = return [x]    -- Plain property, leave it as it is

execute :: PPContext -> String -> NPPValue -> IO [NPPProperty]
execute c "include" (NPPVal y) = do
  file <- IO.readFile y
  --TODO Get current filename, delta with new filename, add to context and update context
  let dc = [("filepath", includepathdiff y )]
  let nc = updateContext c y
  return $ process nc . parse
execute _ x         y          = error $ "Unknown NPP directive \"" ++ x ++ "\" with params: " ++ (show y)

updateContext :: PPContext -> PPContext -> PPContext
updateContext c