{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (second)
import Control.Monad (when)
import Data.Maybe (mapMaybe)
import Distribution.Simple.Utils (tryFindPackageDesc)
import Distribution.PackageDescription hiding (options)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Text (simpleParse)
import Distribution.Verbosity (silent)
import System.Directory (getCurrentDirectory)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.FilePath (makeRelative)
import System.IO (BufferMode(..), hSetBuffering, stdout)

import Options

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
    logInfo opts ("Using cabal manifest: " ++ makeRelative cwd descPath)

    -- Find dependencies - TODO: Output errors on unparsed input, instead
    --                     of this shitty solution
    let deps = mapMaybe simpleParse depNames

    -- Update the package description
    let desc' = desc { condExecutables = map (second (addConstraints deps))
                                             (condExecutables desc)
                     }

    -- Write the updated description to disk
    writeGenericPackageDescription descPath desc'
  where addConstraints toAdd condt =
            condt { condTreeConstraints = toAdd ++ condTreeConstraints condt
                  }

-- |
-- Gets a package description corresponding to a certain directory, parses
-- and returns it
getDirPackageDesc :: FilePath -> IO (GenericPackageDescription, FilePath)
getDirPackageDesc dir = do
    descPath <- tryFindPackageDesc dir
    desc <- readPackageDescription silent descPath
    return (desc, descPath)

logInfo :: Options -> String -> IO ()
logInfo opts x | optionVerbose opts = putStrLn x
               | otherwise          = return ()
