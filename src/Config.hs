{-# LANGUAGE Safe #-}

{-|
Module      : Config
Description : Command line utils
-}
module Config
( Cfg
, ConfigProperty
, readCfg
, add
, merge
) where

import safe qualified System.IO as IO   (readFile)
import safe qualified Data.Char as Char (isSpace)
import safe qualified Data.List as List (elemIndex)

-- | Config properties/values
type Cfg = [ConfigProperty]

-- | Single config property
type ConfigProperty = (String, String)

-- | Add a property to a config
add :: Cfg -> ConfigProperty -> Cfg
add cfg prop = prop : cfg

-- | Merge two configs together
merge :: [Cfg] -> Cfg
merge = concat

-- | Read a config file to its Cfg struct
readCfg :: String -> IO Cfg
readCfg = (map parseCfgLine . lines <$>) . IO.readFile

parseCfgLine :: String -> ConfigProperty
parseCfgLine str =
    (trim key, trim val)
  where
    trim = trimR . trimL
    trimR = takeWhile (not . Char.isSpace)
    trimL = dropWhile Char.isSpace
    key = take sepIndex str
    val = drop (sepIndex+1) str
    sepIndex = getIdx $ List.elemIndex ':' str
    getIdx :: Maybe Int -> Int
    getIdx (Just c) = c
    getIdx _        = error $ "Invalid config line: " ++ str