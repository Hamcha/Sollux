{-# LANGUAGE Safe #-}

module Main where

import safe qualified CLI                       (unknown, unknownHelp)
import safe qualified Nginx                     (execute, help)
import safe qualified System.Environment as Env (getArgs, getProgName)
import safe qualified Config                    (Cfg, add)

usage :: IO ()
usage = Env.getProgName >>=
  \appname -> (putStr . unlines) [
    "Usage: " ++ appname ++ " [<flags>] <command> [<arguments>]",
    "\nCommands:",
    "  npp      Run the Nginx PreProcessor engine (NPP)",
    "  help     Print help pages (like this) about Sollux and its submodules",
    "\nFlags:",
    "  -d       Read config and other files from a specified directory",
    "\nUse \"help <command>\" to get help pages about specific commands and what arguments they might accept"
  ]

help :: [String] -> IO ()
help []         = usage
help ("npp":xs) = Nginx.help xs
help (x:_)      = CLI.unknownHelp x

parseArgs :: Config.Cfg -> [String] -> IO ()
-- No args: send usage
parseArgs _ [] = usage
-- CMD Flags: parse and proceed
parseArgs c ("-d":dir:xs) = parseArgs (Config.add c ("confdir", dir)) xs
-- CMD Commands: execute selected submodule
parseArgs _ ("help":xs) = help xs
parseArgs c ("npp" :xs) = Nginx.execute c xs
parseArgs _ (x     :_ ) = CLI.unknown x

defaultcfg :: Config.Cfg
defaultcfg = [("confdir", "conf")]

main :: IO ()
main = Env.getArgs >>= parseArgs defaultcfg