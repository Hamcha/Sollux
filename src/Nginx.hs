{-# LANGUAGE Safe #-}

{-|
Module      : Nginx
Description : CLI tools for accessing Nginx.* operations (NPP)
-}
module Nginx
( execute
, help
) where

import safe qualified CLI                (unknownSub, unknownSubHelp)
import safe qualified Config             (Cfg, readCfg)
import safe           Nginx.Parser       (parse)
import safe           Nginx.Preprocessor (process)
import safe           Nginx.Compiler     (compile)
import safe qualified System.IO as IO    (readFile)

-- | Help topics for NPP
help :: [String] -> IO ()
help []    = putStrLn "TODO!"
help (x:_) = CLI.unknownSubHelp ["npp"] x

-- | CLI parsing and execution for NPP
execute :: Config.Cfg -> [String] -> IO ()
execute c []    = regen c
execute _ (x:_) = CLI.unknownSub ["npp"] x

must :: Maybe String -> String
must (Just c) = c
must _        = error "Could not find required value!"

regen :: Config.Cfg -> IO ()
regen c = do
  let cfgpath = must $ lookup "confdir" c
  ngcfg <- Config.readCfg (cfgpath ++ "/npp.conf")
  let main = must $ lookup "main" ngcfg
  --putStrLn . compile <$> process . parse <$> IO.readFile (cfgpath ++ main)
  let path = cfgpath ++ "/" ++ main
  file <- IO.readFile path
  let ctx = [("filepath", path)]
  out <- (process ctx . parse) file
  putStrLn $ compile out