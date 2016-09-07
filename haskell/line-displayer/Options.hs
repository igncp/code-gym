module Options (Options, getOptions, file, shouldNotWait, delaySeconds, isHierarchicalFile)
where 

import Data.List (elemIndex)

data Options = Options {
  shouldNotWait :: Bool,
  file :: String,
  delaySeconds :: Int,
  isHierarchicalFile :: Bool
} deriving (Show)

delaySecondsDefault :: Int
delaySecondsDefault = 2

getOptions :: [String] -> Options
getOptions args = Options {
  file = last args,
  shouldNotWait = elem "-x" args,
  isHierarchicalFile = elem "-i" args,
  delaySeconds = case (elemIndex "-d" args) of
    Nothing -> delaySecondsDefault
    Just idx -> read $ args !! (idx + 1)
}
