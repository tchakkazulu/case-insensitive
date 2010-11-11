module System.IO.CaseInsensitive 
  ( -- $descr
    findCIPath
  , findCIPath'
    -- * Wrappers around System.IO functions
    -- | The following are simple wrappers around the functions in
    -- "System.IO" with the same name.
  , readFile
  , writeFile
  , appendFile
  , openFile
  , withFile
  , withBinaryFile
  , openBinaryFile
  ) where

import Control.Monad (foldM)

import Data.Char (toLower)

import Prelude hiding (readFile, writeFile, appendFile )

import System.Directory
import System.FilePath
import System.IO hiding (readFile, writeFile, appendFile, openFile, withFile, withBinaryFile, openBinaryFile)
import qualified System.IO as IO

import System.IO.Error

-- $descr
-- This module is intended to be imported qualified, because of
-- conflicting names with "Prelude" and "System.IO" functions.
--
-- This module does not export functions for working with 'ByteString's
-- or 'Text', but you can define those yourself using 'findCIPath'

icException :: String -> FilePath -> IOError
icException info fp = mkIOError doesNotExistErrorType info Nothing (Just fp)

whenNotExists :: FilePath -> IO (Maybe FilePath) -> IO (Maybe FilePath)
whenNotExists fp action = do
  p <- exists fp
  if p then return (Just fp)
       else action

exists :: FilePath -> IO Bool
exists fp = do
  isFile <- doesFileExist fp
  isDir <- doesDirectoryExist fp
  return (isFile || isDir)

-- | The main functionality. This searches the filesystem for
-- a path that is the same as the argument, except possibly
-- different case conventions.
--
-- The result is 'Nothing' if no such path exists, or if
-- multiple such paths exist.
findCIPath :: FilePath -> IO (Maybe FilePath)
findCIPath fp = whenNotExists fp $ do
  let (drive, path) = splitDrive fp
  foldM findCIChunk (Just drive) (splitDirectories path)

-- | Same as 'findCIPath', but throws an 'isDoesNotExist' exception
-- instead of returning 'Nothing'.
findCIPath' :: FilePath -> IO FilePath
findCIPath' fp = do
  res <- findCIPath fp
  case res of
    Nothing -> ioError $ icException "findCIPath'" fp
    Just x  -> return x

findCIChunk :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
findCIChunk Nothing    _new = return Nothing
findCIChunk (Just "")   new = findCIChunk (Just ".") new
findCIChunk (Just base) new = whenNotExists (base </> new) $ do
  cont <- getDirectoryContents base
  case filter (new ===) cont of
    [x] -> return (Just (base </> x))
    _   -> return Nothing

setErrLoc :: String -> IO a -> IO a
setErrLoc str = modifyIOError (`ioeSetLocation` str)
    
(===) :: String -> String -> Bool
x === y = map toLower x == map toLower y

ci2 :: String -> (FilePath -> a -> IO b) -> FilePath -> a -> IO b
ci2 str f fp x = setErrLoc str $ do
  fp' <- findCIPath' fp
  f fp' x

ci3 :: String -> (FilePath -> a -> b -> IO c) -> FilePath -> a -> b -> IO c
ci3 str f fp x = ci2 str (\fp' -> f fp' x) fp

readFile :: FilePath -> IO String
readFile fp = setErrLoc "readFile" $ findCIPath' fp >>= IO.readFile

writeFile :: FilePath -> String -> IO ()
writeFile = ci2 "writeFile" IO.writeFile

appendFile :: FilePath -> String -> IO ()
appendFile = ci2 "appendFile" IO.appendFile

openFile :: FilePath -> IOMode -> IO Handle
openFile = ci2 "openFile" IO.openFile

withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile = ci3 "withFile" IO.withFile

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withBinaryFile = ci3 "withBinaryFile" IO.withBinaryFile

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = ci2 "openBinaryFile" IO.openBinaryFile

