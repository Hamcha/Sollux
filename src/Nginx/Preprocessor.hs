{-|
Module      : Nginx.Preprocessor
Description : Processes .npp structures to fill in macros, functions etc
-}
module Nginx.Preprocessor
( process
) where

import Nginx.Parser

-- | Process an NPP tree
process :: [NPPProperty] -> [NPPProperty]
process = map processLine

processLine :: NPPProperty -> NPPProperty
processLine (('@':x), y)    = execute x y               -- Preprocessor instruction, handle it
processLine (x, NPPBlock y) = (x, NPPBlock $ process y) -- Recurse processing in blocks
processLine x               = x                         -- Plain property, leave it as it is

execute :: String -> NPPValue -> NPPProperty
execute x y = error $ "Unknown NPP directive \"" ++ x ++ "\" with params: " ++ (show y)