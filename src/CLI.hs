module CLI
( unknown
, unknownSub
, unknownHelp
, unknownSubHelp
) where

unknown :: String -> IO ()
unknown x = putStrLn $ "Unknown command \"" ++ x ++ "\". Use \"help\" to see a list of all available commands"

unknownSub :: [String] -> String -> IO ()
unknownSub x y = putStrLn $ "Unknown command \"" ++ y ++ "\". Use \"help " ++ (unwords x) ++ "\" to see a list of all available commands"

unknownHelp :: String -> IO ()
unknownHelp x = putStrLn $ "Unknown help topic \"" ++ x ++ "\". Use \"help\" to see a list of all available topics"

unknownSubHelp :: [String] -> String -> IO ()
unknownSubHelp x y = putStrLn $ "Unknown help topic \"" ++ y ++ "\". Use \"help " ++ (unwords x) ++ "\" to see a list of all available topics"