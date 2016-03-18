{-# LANGUAGE Safe #-}

{-|
Module      : Nginx
Description : CLI tools for accessing Nginx.* operations (NPP)
-}
module Nginx
( execute
, help
) where

import safe qualified CLI (unknownSub, unknownSubHelp)

-- | Help topics for NPP
help :: [String] -> IO ()
help []    = putStrLn "Oh oh oh!"
help (x:_) = CLI.unknownSubHelp ["npp"] x

-- | CLI parsing and execution for NPP
execute :: [String] -> IO ()
execute (x:_)         = CLI.unknownSub ["npp"] x
--  (putStrLn . compile . process . parse) example