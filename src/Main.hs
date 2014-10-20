{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Arrow (second)
import Control.Monad (foldM, forM_, when)
import Data.Either (partitionEithers)
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription hiding (options)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Simple.Utils (tryFindPackageDesc)
import Distribution.Text (Text(..), simpleParse, display)
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

    case partitionEithers $ map simpleParse' depNames of
        -- TODO - refactor this soup
        ([], deps) -> case optionBuildTargets opts of
            [] ->
              -- Ignore verbosity here, since this is important
              foldM (\curDesc dep -> do
                  putStrLn $ "Adding " ++ display dep ++ " to all targets"
                  return $ addToAllTargets dep curDesc) desc deps >>=
              writeGenericPackageDescription descPath
            _ -> putStrLn "Unimplemented :P" >> exitWith (ExitFailure 1)
        (errs, _) -> forM_ errs putStrLn >> exitWith (ExitFailure 1)

-- |
-- Gets a package description corresponding to a certain directory, parses
-- and returns it
getDirPackageDesc :: FilePath -> IO (GenericPackageDescription, FilePath)
getDirPackageDesc dir = do
    descPath <- tryFindPackageDesc dir
    desc <- readPackageDescription silent descPath
    return (desc, descPath)

-- |
-- An `Either` version of `Distribution.Text.simpleParse`.
simpleParse' :: Text a => String -> Either String a
simpleParse' s = case simpleParse s of
    Just x  -> Right x
    Nothing -> Left $ "Unable to parse " ++ s

-- |
-- Adds a dependency to all targets in a `GenericPackageDescription` and
-- returns it updated
addToAllTargets :: Dependency -> GenericPackageDescription -> GenericPackageDescription
addToAllTargets dep desc =
    -- Update the package description
    desc { condLibrary     = fmap (addConstraint dep) (condLibrary desc)
         , condExecutables = addConstraintToTupleList dep (condExecutables desc)
         , condTestSuites  = addConstraintToTupleList dep (condTestSuites desc)
         , condBenchmarks  = addConstraintToTupleList dep (condBenchmarks desc)
         }
  where addConstraintToTupleList d = fmap (second (addConstraint d))
        addConstraint toAdd condt =
            condt { condTreeConstraints = toAdd : condTreeConstraints condt }

-- |
-- Logs an information message depending on the `Options` verbosity.
logInfo :: Options -> String -> IO ()
logInfo opts x | optionVerbose opts = putStrLn x
               | otherwise          = return ()
