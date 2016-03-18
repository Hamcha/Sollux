{-# LANGUAGE Safe #-}

{-|
Module      : CLI
Description : Command line utils
-}
module CLI
( unknown
, unknownSub
, unknownHelp
, unknownSubHelp
) where

-- | Returns an "unknown command" message
unknown :: String -> IO ()
unknown x = putStrLn $ "Unknown command \"" ++ x ++ "\". Use \"help\" to see a list of all available commands"

-- | Returns an "unknown command" message relative to a submodule
unknownSub :: [String] -> String -> IO ()
unknownSub x y = putStrLn $ "Unknown command \"" ++ y ++ "\". Use \"help " ++ (unwords x) ++ "\" to see a list of all available commands"

-- | Returns an "unknown help topic" message
unknownHelp :: String -> IO ()
unknownHelp x = putStrLn $ "Unknown help topic \"" ++ x ++ "\". Use \"help\" to see a list of all available topics"

-- | Returns an "unknown help topic" message relative to a submodule
unknownSubHelp :: [String] -> String -> IO ()
unknownSubHelp x y = putStrLn $ "Unknown help topic \"" ++ y ++ "\". Use \"help " ++ (unwords x) ++ "\" to see a list of all available topics"