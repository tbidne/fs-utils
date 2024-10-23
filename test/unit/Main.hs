{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Control.Monad (when)
import FileSystem.OsPath (OsPath, osp, (</>))
import FileSystem.OsPath qualified as FS.OsPath
import System.Directory.OsPath qualified as Dir
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import Test.Tasty (defaultMain, testGroup, withResource)
import Unit.FileSystem.IO qualified
import Unit.FileSystem.OsPath qualified

main :: IO ()
main =
  defaultMain $
    withResource setup teardown $ \args ->
      testGroup
        "Unit Tests"
        [ Unit.FileSystem.IO.tests args,
          Unit.FileSystem.OsPath.tests
        ]

setup :: IO OsPath
setup = do
  tmpDir <-
    (\s -> s </> [osp|fs-utils|] </> [osp|unit|])
      <$> Dir.getTemporaryDirectory

  exists <- Dir.doesDirectoryExist tmpDir
  when exists $ Dir.removeDirectoryRecursive tmpDir
  Dir.createDirectoryIfMissing True tmpDir

  pure tmpDir

teardown :: OsPath -> IO ()
teardown fp = guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup
  where
    cleanup = Dir.removePathForcibly fp
    doNothing =
      putStrLn $ "*** Not cleaning up tmp dir: " <> FS.OsPath.decodeLenient fp
