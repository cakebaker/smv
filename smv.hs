import System.Environment (getArgs)
import System.Directory (doesFileExist, getDirectoryContents, copyFile, removeFile)
import System.FilePath ((</>))
import System.IO (hSetEcho, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))
import Control.Monad

main :: IO ()
main = do
  initialize
  args <- getArgs
  run args

initialize :: IO ()
initialize = do
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

run :: [String] -> IO ()
run (source:target:[]) = do 
  filenames <- getFilenames source
  moveFiles filenames source target
run _ = showHelp

moveFiles :: [FilePath] -> FilePath -> FilePath -> IO ()
moveFiles [] _ _ = return ()
moveFiles (filename:rest) source target = do
  canMove <- shouldMoveFile filename
  when canMove (moveFile filename source target)
  moveFiles rest source target

shouldMoveFile :: FilePath -> IO Bool
shouldMoveFile filename = do
  putStr("move " ++ filename ++ "? ")
  answer <- getChar
  putStrLn [answer]
  return (if answer == 'y' then True else False)

moveFile :: FilePath -> FilePath -> FilePath -> IO ()
moveFile filename source target = do
  copyFile (source </> filename) (target </> filename)
  removeFile (source </> filename)

getFilenames :: FilePath -> IO [FilePath]
getFilenames dir = do
  dirContent <- getDirectoryContents dir
  filterM (\filename -> doesFileExist (dir </> filename)) dirContent

showHelp :: IO ()
showHelp = do
  putStrLn "Usage: smv SOURCE TARGET"
  putStrLn "Move files from SOURCE to TARGET, prompt before move of each file."
