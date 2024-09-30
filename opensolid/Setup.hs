import Distribution.Simple qualified
import System.Directory qualified
import System.FilePath qualified as FilePath
import Distribution.Types.LocalBuildInfo qualified as LocalBuildInfo
import Distribution.Types.PackageDescription qualified as PackageDescription
import Distribution.Types.Library qualified as Library
import Distribution.Types.BuildInfo qualified as BuildInfo
import System.Exit qualified
import System.Process qualified

{-| Get the path to the opensolid-jit directory -}
getJitDir :: IO String
getJitDir = do
  cwd <- System.Directory.getCurrentDirectory
  let parentDir = FilePath.takeDirectory cwd
  return (FilePath.joinPath [parentDir, "opensolid-jit"])

{-| Build the opensolid-jit library with Cargo -}
buildJitLibrary :: IO ()
buildJitLibrary = do
  jitDir <- getJitDir
  let command = System.Process.proc "cargo" ["build", "--release"]
  (_, _, _, process) <- System.Process.createProcess command{System.Process.cwd = Just jitDir}
  exitCode <- System.Process.waitForProcess process
  if exitCode == System.Exit.ExitSuccess
    then return ()
    else fail "failed to build opensolid-jit library"

main :: IO ()
main =
  Distribution.Simple.defaultMainWithHooks $
    Distribution.Simple.simpleUserHooks
      { Distribution.Simple.confHook =
          \( genericPackageDescription, hookedBuildInfo ) configFlags -> do
            -- Build the opensolid-jit library first,
            -- since Cabal will check for it during configuration
            -- (because it's listed under extra-libraries in opensolid.cabal)
            buildJitLibrary
            -- Add Rust's library output directory as an extra lib dir
            jitDir <- getJitDir
            let rustLibDir = FilePath.joinPath [jitDir, "target", "release"]
            let defaultConfHook = Distribution.Simple.confHook Distribution.Simple.simpleUserHooks
            localBuildInfo <- defaultConfHook ( genericPackageDescription, hookedBuildInfo ) configFlags
            let packageDescription = LocalBuildInfo.localPkgDescr localBuildInfo
            Just library <- return (PackageDescription.library packageDescription)
            let buildInfo = Library.libBuildInfo library
            let extraLibDirs = BuildInfo.extraLibDirs buildInfo
            let updatedExtraLibDirs = rustLibDir : extraLibDirs
            let updatedBuildInfo = buildInfo{BuildInfo.extraLibDirs = updatedExtraLibDirs}
            let updatedLibrary = library{Library.libBuildInfo = updatedBuildInfo}
            let updatedPackageDescription = packageDescription{PackageDescription.library = Just updatedLibrary}
            let updatedLocalBuildInfo = localBuildInfo{LocalBuildInfo.localPkgDescr = updatedPackageDescription}
            return updatedLocalBuildInfo
      , Distribution.Simple.preBuild =
          \args buildFlags -> do
            -- Build the opensolid-jit library first
            buildJitLibrary
            let defaultPreBuildHook = Distribution.Simple.preBuild Distribution.Simple.simpleUserHooks
            defaultPreBuildHook args buildFlags
      }
