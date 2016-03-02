module Main where

import qualified Nginx.Parser

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

main :: IO ()
main = do
  print $ Nginx.Parser.parse example