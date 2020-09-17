module Path.Extra
    ( reversePosixPath
    ) where

import Control.Exception (assert)
import qualified System.FilePath.Posix as FPP
import Path


-- | For given path P, returns path P', such that (terminal pseudocode incoming)
-- `pwd == (cd P && cd P' && pwd)`, or to put it differently, such that
-- `cd P && cd P'` is noop (does nothing).
-- It will always be either "." (only if input is ".") or a series of ".."
-- (e.g. reversePath [reldir|foo/bar|] == "../..").
reversePosixPath :: Path Rel Dir -> FilePath
reversePosixPath path
    | length parts == 0 = "."
    | otherwise         = assert (not (".." `elem` parts)) $
                          FPP.joinPath $ map (const "..") parts
  where
    parts :: [String]
    parts = filter (not . (== ".")) $ FPP.splitDirectories $ toFilePath path
