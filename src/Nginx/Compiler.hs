{-|
Module      : Nginx.Compiler
Description : Compiles parsed and processed NPP templates into raw nginx configuration files
-}
module Nginx.Compiler
( compile
) where

import qualified Data.List    as List
import           Nginx.Parser

compile :: [NPPProperty] -> String
compile = (List.intercalate "\n") . compileProps

flatten :: [NPPValue] -> String
flatten []            = ""
flatten (NPPVal x:[]) = x
flatten (NPPVal x:xs) = x ++ flatten xs
flatten (x:xs)        = error "Parsing error! Non NPPVal in NPPList: " ++ (show x)

indent :: String -> String
indent str = '\t' : str

compileProps :: [NPPProperty] -> [String]
compileProps [] = []
compileProps ((key, NPPVoid     ):xs) = (key                             ++ ";") : compileProps xs
compileProps ((key, NPPVal   val):xs) = ((key ++ " " ++ val)             ++ ";") : compileProps xs
compileProps ((key, NPPList  lst):xs) = ((key ++ " " ++ flatten lst)     ++ ";") : compileProps xs
compileProps ((key, NPPBlock blk):xs) =
  (key ++ " {") : (map indent $ compileProps blk) ++ "}" : compileProps xs
compileProps ((key, x):_) =
  error $ "Parsing error! Unknown NPPValue in property " ++ key ++ ": " ++ (show x)