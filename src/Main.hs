{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (forM_, when)
import Data.Text (Text(..))
import Distribution.Simple.Utils (tryFindPackageDesc)
import Distribution.Package (Dependency(..), PackageName(..))
import Distribution.PackageDescription hiding (options)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Text (simpleParse)
import Distribution.Verbosity (silent)
import Distribution.Version (anyVersion)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..),
                             getOpt, usageInfo)
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath (makeRelative)
import System.IO (BufferMode(..), hSetBuffering, stdout)

version :: String
version = "0.1.0.0"

-- |
-- The entry point
main :: IO ()
main = do
    -- Use line buffering for better responsiveness
    hSetBuffering stdout LineBuffering
    (opts, depNames) <- getOptions

    -- Forever boring command-line interface handling
    when (optionVersion opts) $ putStrLn version >> exitSuccess
    when (optionHelp opts) $ putStr usage >> exitSuccess
    when (null depNames) $ putStr usage >> exitWith (ExitFailure 1)

    -- Read the package description
    cwd <- getCurrentDirectory
    (desc, descPath) <- getDirPackageDesc cwd
    logInfo opts ("Using cabal manifest: " ++ makeRelative cwd (descPath))

    -- Find dependencies
    deps <- mapM findDependency depNames

    -- Update the package description
    let desc' = desc { condExecutables = map (\(f, s) -> (f, addConstraints deps s))
                                             (condExecutables desc)
                     }

    -- Write the updated description to disk
    writeGenericPackageDescription descPath desc'
  where addConstraints toAdd condt =
            condt { condTreeConstraints = toAdd ++ condTreeConstraints condt
                  }

data Options = Options { optionVerbose :: Bool
                       , optionVersion :: Bool
                       , optionHelp :: Bool
                       , optionBuildTarget :: Maybe String
                       }
  deriving(Eq, Show)

defaultOptions :: Options
defaultOptions = Options { optionVerbose = False
                         , optionVersion = False
                         , optionHelp = False
                         , optionBuildTarget = Nothing
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
          , Option "t" ["target"]
              (ReqArg (\arg opts -> opts { optionBuildTarget = Just arg}) "target")
              "The build target to add dependencies to"
          ]

getOptions :: IO (Options, [String])
getOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (opts, deps, []) -> return (foldr ($) defaultOptions opts, deps)
        (_, _, errs) -> putStr (concat errs ++ usage) >> exitWith (ExitFailure 1)

logInfo :: Options -> String -> IO ()
logInfo opts x | optionVerbose opts = putStrLn x
               | otherwise          = return ()

-- |
-- Gets a package description corresponding to a certain directory, parses
-- and returns it.
getDirPackageDesc :: FilePath -> IO (GenericPackageDescription, FilePath)
getDirPackageDesc dir = do
    descPath <- tryFindPackageDesc dir
    desc <- readPackageDescription silent descPath
    return (desc, descPath)

findDependency :: String -> IO Dependency
findDependency _ = do
    case simpleParse ("hzulip") of
        Just packageName ->
            return $ Dependency (packageName) (anyVersion)
        Nothing ->
            fail $ "Failed to parse "
