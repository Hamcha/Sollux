{-# LANGUAGE Safe #-}

module Main where

import safe qualified CLI                       (unknown, unknownHelp)
import safe qualified Nginx                     (execute, help)
import safe qualified System.Environment as Env (getArgs, getProgName)

usage :: IO ()
usage = Env.getProgName >>=
  \appname -> (putStr . unlines) [
    "Usage: " ++ appname ++ " <command> [<arguments>]"
  ]

help :: [String] -> IO ()
help []         = usage
help ("npp":xs) = Nginx.help xs
help (x:_)      = CLI.unknownHelp x

parseArgs :: [String] -> IO ()
parseArgs []          = usage
parseArgs ("help":xs) = help xs
parseArgs ("npp" :xs) = Nginx.execute xs
parseArgs (x     :_ ) = CLI.unknown x

main :: IO ()
main = Env.getArgs >>= parseArgs