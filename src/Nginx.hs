module Nginx
( execute
, help
) where

import qualified CLI (unknownSub, unknownSubHelp)

help :: [String] -> IO ()
help []    = putStrLn "Oh oh oh!"
help (x:_) = CLI.unknownSubHelp ["npp"] x

execute :: [String] -> IO ()
execute (x:_)         = CLI.unknownSub ["npp"] x