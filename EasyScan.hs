import System.Directory
import Data.List (isInfixOf, isPrefixOf)
import Debug.Trace (trace)

main = do
    currentPath <- getCurrentDirectory
    let path = currentPath ++ "/translation"
    filenames <- filter (".po" `isInfixOf`) <$> getDirectoryContents path
    mapM (\file -> readFile file >>= putStrLn . judge file . lines) $ map (addPath path) filenames

addPath path file = path ++ "/" ++ file

judge :: FilePath -> [String] -> String
judge file cs = showOK (judge' cs) ++ "\t : " ++ file where
    judge' = all closed . filter isMsg . zip [1..]

showOK True = "o    "
showOK False = "error"

isMsg (_,msg) = "msgid" `isPrefixOf` msg || "msgstr" `isPrefixOf` msg || "\"" `isPrefixOf` msg

closed (i,msg) = if last msg == '\"'
    then True
    else trace ("line" ++ show i ++ "has not closed.") False