{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides utilities for working with 'Path'.
--
-- @since 0.1
module FileSystem.Path
  ( -- * Pattern Synonym
    pattern MkPath,

    -- * QuasiQuoters
    absdirPathSep,
    absfilePathSep,
    reldirPathSep,
    relfilePathSep,

    -- * Operations
    (<</>>),
    addExtension,
    splitExtension,
    fileExtension,
    replaceExtension,

    -- * Parsing
    parseAbsDir,
    parseAbsFile,
    parseRelDir,
    parseRelFile,

    -- * Elimination
    toOsPath,

    -- * Re-exports
    Path,
    Abs,
    Rel,
    Dir,
    File,
  )
where

import Control.Monad.Catch (MonadThrow)
import Data.Bifunctor (Bifunctor (second))
import FileSystem.Internal qualified as Internal
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Quote
  ( QuasiQuoter
      ( QuasiQuoter,
        quoteDec,
        quoteExp,
        quotePat,
        quoteType
      ),
  )
import OsPath (Abs, Dir, File, Path, Rel)
import OsPath qualified as Path
import System.OsPath (OsPath)
import System.OsString.Internal.Types (OsString (OsString, getOsString))

-- | Pattern synonym for eliminating 'Path' to 'OsPath'.
--
-- @since 0.1
pattern MkPath :: OsPath -> Path b t
pattern MkPath p <- (toOsPath -> p)

{-# COMPLETE MkPath #-}

-- | Like 'Path.absdir', except it runs paths through a "replace function"
-- first. On unix, replaces @\\@ with @/@. On windows, does the opposite.
--
-- @since 0.1
absdirPathSep :: QuasiQuoter
absdirPathSep =
  QuasiQuoter
    { quoteExp = Path.absdir.quoteExp . Internal.replaceSlashes,
      quotePat = Path.absdir.quotePat . Internal.replaceSlashes,
      quoteType = Path.absdir.quoteType . Internal.replaceSlashes,
      quoteDec = Path.absdir.quoteDec . Internal.replaceSlashes
    }

-- | Like 'Path.absfile', except it runs paths through a "replace function"
-- first. On unix, replaces @\\@ with @/@. On windows, does the opposite.
--
-- @since 0.1
absfilePathSep :: QuasiQuoter
absfilePathSep =
  QuasiQuoter
    { quoteExp = Path.absfile.quoteExp . Internal.replaceSlashes,
      quotePat = Path.absfile.quotePat . Internal.replaceSlashes,
      quoteType = Path.absfile.quoteType . Internal.replaceSlashes,
      quoteDec = Path.absfile.quoteDec . Internal.replaceSlashes
    }

-- | Like 'Path.reldir', except it runs paths through a "replace function"
-- first. On unix, replaces @\\@ with @/@. On windows, does the opposite.
--
-- @since 0.1
reldirPathSep :: QuasiQuoter
reldirPathSep =
  QuasiQuoter
    { quoteExp = Path.reldir.quoteExp . Internal.replaceSlashes,
      quotePat = Path.reldir.quotePat . Internal.replaceSlashes,
      quoteType = Path.reldir.quoteType . Internal.replaceSlashes,
      quoteDec = Path.reldir.quoteDec . Internal.replaceSlashes
    }

-- | Like 'Path.relfile', except it runs paths through a "replace function"
-- first. On unix, replaces @\\@ with @/@. On windows, does the opposite.
--
-- @since 0.1
relfilePathSep :: QuasiQuoter
relfilePathSep =
  QuasiQuoter
    { quoteExp = Path.relfile.quoteExp . Internal.replaceSlashes,
      quotePat = Path.relfile.quotePat . Internal.replaceSlashes,
      quoteType = Path.relfile.quoteType . Internal.replaceSlashes,
      quoteDec = Path.relfile.quoteDec . Internal.replaceSlashes
    }

-- | 'Path' to 'OsPath'.
--
-- @since 0.1
toOsPath :: Path b t -> OsPath
toOsPath = OsString . Path.toOsPath

-- | Like 'Path.parseAbsDir', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
parseAbsDir :: (HasCallStack, MonadThrow m) => OsPath -> m (Path Abs Dir)
parseAbsDir = Path.parseAbsDir . (.getOsString)

-- | Like 'Path.parseAbsFile', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
parseAbsFile :: (HasCallStack, MonadThrow m) => OsPath -> m (Path Abs File)
parseAbsFile = Path.parseAbsFile . (.getOsString)

-- | Like 'Path.parseRelDir', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
parseRelDir :: (HasCallStack, MonadThrow m) => OsPath -> m (Path Rel Dir)
parseRelDir = Path.parseRelDir . (.getOsString)

-- | Like 'Path.parseRelFile', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
parseRelFile :: (HasCallStack, MonadThrow m) => OsPath -> m (Path Rel File)
parseRelFile = Path.parseRelFile . (.getOsString)

-- | Alias for 'Path.</>', intended to allow unqualified usage alongside
-- 'OsPath''s @\</\>@.
--
-- @since 0.1
(<</>>) :: Path b Dir -> Path Rel t -> Path b t
(<</>>) = (Path.</>)

infixr 5 <</>>

-- | Like 'Path.addExtension', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
addExtension ::
  (HasCallStack, MonadThrow m) =>
  OsPath ->
  Path b File ->
  m (Path b File)
addExtension p = Path.addExtension p.getOsString

-- | Like 'Path.splitExtension', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
splitExtension ::
  (HasCallStack, MonadThrow m) =>
  Path b File ->
  m (Path b File, OsPath)
splitExtension = fmap (second OsString) . Path.splitExtension

-- | Like 'Path.fileExtension', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
fileExtension :: (HasCallStack, MonadThrow m) => Path b File -> m OsPath
fileExtension = fmap OsString . Path.fileExtension

-- | Like 'Path.replaceExtension', but in terms of 'OsPath' rather than system
-- specific type.
--
-- @since 0.1
replaceExtension ::
  (HasCallStack, MonadThrow m) =>
  OsPath ->
  Path b File ->
  m (Path b File)
replaceExtension p = Path.replaceExtension p.getOsString
