-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2021 #-}

import Distribution.Simple qualified
import Distribution.Types.BuildInfo qualified as BuildInfo
import Distribution.Types.Library qualified as Library
import Distribution.Types.LocalBuildInfo qualified as LocalBuildInfo
import Distribution.Types.PackageDescription qualified as PackageDescription
import System.Directory qualified
import System.Exit qualified
import System.FilePath qualified as FilePath
import System.Process qualified

-- | Get the path to the opensolid-rs directory
getRustLibraryDir :: IO String
getRustLibraryDir = do
  cwd <- System.Directory.getCurrentDirectory
  let parentDir = FilePath.takeDirectory cwd
  return (FilePath.joinPath [parentDir, "opensolid-rs"])

-- | Build the opensolid-rs library with Cargo
buildRustLibrary :: IO ()
buildRustLibrary = do
  rustLibraryDir <- getRustLibraryDir
  let command = System.Process.proc "cargo" ["build", "--release"]
  (_, _, _, process) <-
    System.Process.createProcess command{System.Process.cwd = Just rustLibraryDir}
  exitCode <- System.Process.waitForProcess process
  if exitCode == System.Exit.ExitSuccess
    then return ()
    else fail "failed to build opensolid-rs library"

main :: IO ()
main =
  Distribution.Simple.defaultMainWithHooks $
    Distribution.Simple.simpleUserHooks
      { Distribution.Simple.confHook =
          \(genericPackageDescription, hookedBuildInfo) configFlags -> do
            -- Build the opensolid-rs library first,
            -- since Cabal will check for it during configuration
            -- (because it's listed under extra-libraries in opensolid.cabal)
            buildRustLibrary
            -- Add Rust's library output directory as an extra lib dir
            rustLibraryDir <- getRustLibraryDir
            let rustTargetReleaseDir = FilePath.joinPath [rustLibraryDir, "target", "release"]
            let defaultConfHook = Distribution.Simple.confHook Distribution.Simple.simpleUserHooks
            localBuildInfo <- defaultConfHook (genericPackageDescription, hookedBuildInfo) configFlags
            let packageDescription = LocalBuildInfo.localPkgDescr localBuildInfo
            Just library <- return (PackageDescription.library packageDescription)
            let buildInfo = Library.libBuildInfo library
            let extraLibDirs = BuildInfo.extraLibDirs buildInfo
            let updatedExtraLibDirs = rustTargetReleaseDir : extraLibDirs
            let updatedBuildInfo = buildInfo{BuildInfo.extraLibDirs = updatedExtraLibDirs}
            let updatedLibrary = library{Library.libBuildInfo = updatedBuildInfo}
            let updatedPackageDescription = packageDescription{PackageDescription.library = Just updatedLibrary}
            let updatedLocalBuildInfo = localBuildInfo{LocalBuildInfo.localPkgDescr = updatedPackageDescription}
            return updatedLocalBuildInfo
      , Distribution.Simple.preBuild =
          \args buildFlags -> do
            -- Build the opensolid-rs library first
            buildRustLibrary
            let defaultPreBuildHook = Distribution.Simple.preBuild Distribution.Simple.simpleUserHooks
            defaultPreBuildHook args buildFlags
      }
