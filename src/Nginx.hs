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
import safe qualified Config             (Cfg, loadCfg)
import safe           Nginx.Parser       (mustParse)
import safe           Nginx.Preprocessor (PPValue(..), mustProcess)
import safe           Nginx.Compiler     (compile)
import safe qualified System.IO as IO    (readFile, writeFile)
import safe           Utils              (abspath, must)

-- | Help topics for NPP
help :: [String] -> IO ()
help []    = putStrLn "TODO!"
help (x:_) = CLI.unknownSubHelp ["npp"] x

-- | CLI parsing and execution for NPP
execute :: Config.Cfg -> [String] -> IO ()
execute c []    = Config.loadCfg "/npp.conf" c >>= regen
execute _ (x:_) = CLI.unknownSub ["npp"] x

regen :: Config.Cfg -> IO ()
regen cfg =
  IO.readFile path
    >>= mustProcess ctx . mustParse path
    >>= IO.writeFile outpath . compile
  where
    ctx     = [("filepath", PPVar path)]
    outpath = abspath cfgpath outrel
    path    = abspath cfgpath main
    outrel  = get "out"
    cfgpath = get "confdir"
    main    = get "main"
    get str = must $ lookup str cfg
