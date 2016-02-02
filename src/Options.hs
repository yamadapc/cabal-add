module Options where

import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..),
                             getOpt, usageInfo)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

data Options = Options { optionVerbose :: Bool
                       , optionVersion :: Bool
                       , optionHelp :: Bool
                       , optionBuildTargets :: [String]
                       }
  deriving(Eq, Show)

defaultOptions :: Options
defaultOptions = Options { optionVerbose = False
                         , optionVersion = False
                         , optionHelp = False
                         , optionBuildTargets = []
                         }

usage :: String
usage = usageInfo header options
  where header = "Usage: cabal-add [options...] dependencies..."

options :: [OptDescr (Options -> Options)]
options = [ Option "v" ["verbose"]
              (NoArg (\opts -> opts { optionVerbose = True }))
              "Be verbose"
          , Option "V" ["version"]
              (NoArg (\opts -> opts { optionVersion = True}))
              "Print version and exit"
          , Option "h" ["help"]
              (NoArg (\opts -> opts { optionHelp = True}))
              "Print this message and exit"
          , Option "t" ["targets"]
              (ReqArg
                (\arg opts -> opts { optionBuildTargets = splitCommas arg
                                   })
                "targets")
              "The build targets to add dependencies to separated by commas"
          ]

getOptions :: IO (Options, [String])
getOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (opts, deps, []) -> return (foldr ($) defaultOptions opts, deps)
        (_, _, errs) -> putStr (concat errs ++ usage) >> exitWith (ExitFailure 1)

splitCommas :: String -> [String]
splitCommas = foldr helper []
  where helper ',' xs   = "":xs
        helper c (x:xs) = (c:x):xs
        helper c []     = [c:""]