module WaspIgnoreFile
    ( WaspIgnoreFile
    , parseWaspIgnoreFile
    , readWaspIgnoreFile
    , ignores
    ) where

import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)
import StrongPath (Path, Abs, File, toFilePath)
import System.FilePath.Glob (Pattern, compile, match)

newtype WaspIgnoreFile = WaspIgnoreFile [Pattern]

{-|
  Parses a string to a 'WaspIgnoreFile'.

  An ignore file contains lines that are one of:
  * blank
  * comments (starting with '#')
  * a pattern

  Patterns are glob 'Pattern's, for full details "System.FilePath.Glob". A
  brief description is:

  [@?@] Matches any single character except slashes.
  [@*@] Matches a string of at least 1 character, excluding slashes.
  [@[xyz\]@] Matches a single character in the set `xyz`.
  [@[^xyz\]@] Matches a single character not in the set `xyz`.
  [@**/@] Matches a string of at least 1 character, including slashes.
-}
parseWaspIgnoreFile :: String -> WaspIgnoreFile
parseWaspIgnoreFile = WaspIgnoreFile . map compile . filter isPatternLine . lines
    where
        -- | A line is a pattern line if it's not a comment (doesn't start with
        --   '#') and it's not a blank line
        isPatternLine :: String -> Bool
        isPatternLine [] = False
        isPatternLine ('#':_) = False
        isPatternLine _ = True

{-|
  Reads and parses the wasp ignore file. See 'parseIgnoreFile' for details of
  the file format, but it is very similar to `.gitignore`'s format.

  If the ignore file does not exist, it is interpreted as a blank file.
-}
readWaspIgnoreFile :: Path Abs File -> IO WaspIgnoreFile
readWaspIgnoreFile fp = do
    text <- readFile (toFilePath fp)
            `catch` (\e -> if isDoesNotExistError e then return ""
                           else ioError e)
    return $ parseWaspIgnoreFile text

{-|
  Tests whether a file should be ignored according to an IgnoreFile.

  Example:

  @
  let ignoreFile = parseIgnoreFile "**/.tmp"
  ignoreFile `ignores` "out.tmp" -- True
  ignoreFile `ignores` "src/a.tmp" -- True
  ignoreFile `ignores` "src/a.js" -- False
  @
-}
ignores :: WaspIgnoreFile -> FilePath -> Bool
ignores (WaspIgnoreFile pats) fp = any (`match` fp) pats