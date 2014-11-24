{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (foldM, forM_, when)
import Data.Either (partitionEithers)
import Distribution.Compiler (buildCompilerFlavor)
-- import Distribution.Client.IndexUtils (getSourcePackages)
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription hiding (options)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Simple.Compiler (PackageDB(..))
import Distribution.Simple.Configure (getInstalledPackages, configCompilerEx)
import Distribution.Simple.PackageIndex (PackageIndex, lookupPackageName, merge)
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Utils (tryFindPackageDesc)
import Distribution.Text (Text(..), simpleParse, display)
import Distribution.Verbosity (silent)
import Distribution.Version (anyVersion, withinVersion)
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
        ([], deps) -> do
            deps' <- mapM resolveDependencyVersion deps
            actionAddDependencies opts deps' desc descPath
        (errs, _) -> forM_ errs putStrLn >> exitWith (ExitFailure 1)

-- |
-- If no version is specified by the user, try narrowing down the version
-- to the available packages and return an updated, narrower dependency
resolveDependencyVersion :: Dependency -> IO Dependency
resolveDependencyVersion (Dependency name v) | v == anyVersion = do
    let dbStack = [GlobalPackageDB]
    (compiler, _, configuration) <- configCompilerEx (Just buildCompilerFlavor)
                                                     Nothing
                                                     Nothing
                                                     defaultProgramConfiguration
                                                     silent

    installedPackages <- getInstalledPackages silent
                                              compiler
                                              dbStack
                                              configuration
    sourcePackages <- getSourcePackages dbStack

    let packages = installedPackages `merge` sourcePackages
        infos = lookupPackageName packages name
        versions = map fst infos

    if null versions
        then putStrLn ("Couldn't find " ++ display name) >>
             exitWith (ExitFailure 1)
        else let bestVersion = foldr1 biggerIsBetter versions
               in return $ Dependency name (withinVersion bestVersion)
  -- This is just an example strategy for finding the best package
  where biggerIsBetter c m = if c > m then c else m
resolveDependencyVersion dep = return dep

-- |
-- Get's packages from a PackageDB stack as a Lazy Map (not implemented)
getSourcePackages :: [PackageDB] -> IO PackageIndex
getSourcePackages = undefined

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
addConstraint :: Eq a => a -> CondTree v [a] a1 -> CondTree v [a] a1
addConstraint toAdd condt = condt { condTreeConstraints = constraints' }
  where constraints  = filter (/= toAdd) (condTreeConstraints condt)
        constraints' = toAdd : constraints

-- |
-- `fmap`ed version of `addConstraint`
addConstraintF :: (Eq a, Functor f) => a -> f (CondTree v [a] a1) ->
                                            f (CondTree v [a] a1)
addConstraintF d = fmap (addConstraint d)

-- |
-- Doubly `fmap`ed version of `addConstraint`
addConstraintFF :: (Eq a, Functor f1, Functor f2) => a ->
                                                     f2 (f1 (CondTree v [a] a1)) ->
                                                     f2 (f1 (CondTree v [a] a1))
addConstraintFF d = fmap (addConstraintF d)

-- |
-- Logs an information message depending on the `Options` verbosity.
logInfo :: Options -> String -> IO ()
logInfo opts x | optionVerbose opts = putStrLn x
               | otherwise          = return ()
