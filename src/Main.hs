module Main where

import qualified CLI                       (unknown, unknownHelp)
import qualified Nginx                     (execute, help)
import qualified System.Environment as Env (getArgs, getProgName)

example :: String
example = unlines [
  "worker_processes 1  ",
  "",
  "events:",
  "\tworker_connections 1024",
  "",
  "http:",
  "\tinclude mime.types",
  "\tinclude blacklist.conf",
  "",
  "\tdefault_type application/octet-stream",
  "\tsendfile on",
  "\tkeepalive_timeout 65",
  "\tserver_tokens off",
  "\tgzip on",
  "\t",
  "\ttypes_hash_max_size 2048",
  "\tserver_names_hash_bucket_size 64",
  "",
  "\t@include sites/*"]

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
--  (putStrLn . compile . process . parse) example