{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

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
-- The main action
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

    -- Parse passed-in dependency names and add them to the manifest
    case partitionEithers $ map simpleParse' depNames of
        ([], deps) -> actionAddDependencies opts deps desc descPath
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
-- The action for adding dependencies
actionAddDependencies :: Options -> [Dependency] ->
                         GenericPackageDescription -> FilePath -> IO ()
actionAddDependencies opts deps desc descPath = case optionBuildTargets opts of
    -- TODO
    -- * Don't re-evaluate `display dep` for every iteration
    -- * Don't repeat the `foldM ... >>= ...` code
    -- * Consider adding all dependencies at once, since this is just
    --   written like this because it'd be harder to add logging the other
    --   way
    -- Ignore verbosity here, since this is important
    [] -> foldM (\curDesc dep -> do
              putStrLn $ "Adding " ++ display dep ++ " to all targets"
              return $ addToAllTargets dep curDesc) desc deps >>=
          writeGenericPackageDescription descPath
    ts -> foldM (\curDesc dep -> do
              putStrLn $ "Adding " ++ display dep ++ " to targets " ++
                         concatMap (++ " ") ts -- Ugly, I know...
              return $ addToTargets ts dep curDesc) desc deps >>=
          writeGenericPackageDescription descPath

-- |
-- Adds a dependency to all targets in a `GenericPackageDescription` and
-- returns it updated
addToAllTargets :: Dependency -> GenericPackageDescription ->
                   GenericPackageDescription
addToAllTargets dep desc =
    -- Update the package description
    -- TODO - Ask people if this is readable. I only got to it because of the
    --        type system.
    -- (I had the idea for the first `fmap` generalization, but `fmap . fmap`
    -- was more of an experiment that turned out to work)
    desc { condLibrary     = addConstraintF  dep (condLibrary desc)
         , condExecutables = addConstraintFF dep (condExecutables desc)
         , condTestSuites  = addConstraintFF dep (condTestSuites  desc)
         , condBenchmarks  = addConstraintFF dep (condBenchmarks  desc)
         }

-- |
-- Adds a dependency to a set of targets in a `GenericPackageDescription` and
-- returns it updated
addToTargets :: [String] -> Dependency -> GenericPackageDescription ->
                GenericPackageDescription
addToTargets [] _ desc = desc
addToTargets (t:ts) dep desc | t == "library" = addToTargets ts dep desc'
  where desc' = desc { condLibrary = addConstraintF dep (condLibrary desc) }
addToTargets (t:ts) dep desc = addToTargets ts dep desc'
  -- I tried to make this the least confusing I could, but it still sucks.
  -- There must be a cleaner, better way to write it
  where guardedAddConstraint :: (String, CondTree v [Dependency] b) ->
                                (String, CondTree v [Dependency] b)
        guardedAddConstraint x = if fst x == t
                                    then addConstraintF dep x
                                    else x
        guardedAddConstraintF = fmap guardedAddConstraint
        exs' = guardedAddConstraintF (condExecutables desc)
        tss' = guardedAddConstraintF (condTestSuites  desc)
        bcs' = guardedAddConstraintF (condBenchmarks  desc)
        desc' = desc { condExecutables = exs'
                     , condTestSuites  = tss'
                     , condBenchmarks  = bcs'
                     }

-- |
-- Adds a dependency constraint to a build node
addConstraint :: a -> CondTree v [a] a1 -> CondTree v [a] a1
addConstraint toAdd condt =
    condt { condTreeConstraints = toAdd : condTreeConstraints condt }

-- |
-- `fmap`ed version of `addConstraint`
addConstraintF :: Functor f => a -> f (CondTree v [a] a1) ->
                                    f (CondTree v [a] a1)
addConstraintF d = fmap (addConstraint d)

-- |
-- Doubly `fmap`ed version of `addConstraint`
addConstraintFF :: (Functor f1, Functor f2) => a -> f2 (f1 (CondTree v [a] a1)) ->
                                                    f2 (f1 (CondTree v [a] a1))
addConstraintFF d = fmap (addConstraintF d)

-- |
-- Logs an information message depending on the `Options` verbosity.
logInfo :: Options -> String -> IO ()
logInfo opts x | optionVerbose opts = putStrLn x
               | otherwise          = return ()
