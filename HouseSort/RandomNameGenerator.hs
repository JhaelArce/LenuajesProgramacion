module RandomNamePick where

import System.IO
import System.Random
import Control.Monad (liftM)
import qualified Data.Text    as Text 
import qualified Data.Text.IO as Text  


type StudentName = String

data Student = Student {
    lastName :: String,
    name :: StudentName
} deriving (Show, Eq, Ord)

data SchoolHouse = Griffindor
                 | Hufflepuff
                 | Ravenclaw
                 | Slytherin
                 deriving (Eq, Show, Ord, Read)

maxStudentsPerHouse :: Int
maxStudentsPerHouse = 1

countStudents :: SchoolHouse -> [(SchoolHouse, Int)] -> Int
countStudents house [] = 0
countStudents house ((h, n):rest)
    | house == h = n
    | otherwise = countStudents house rest

updateCounts :: SchoolHouse -> [(SchoolHouse, Int)] -> [(SchoolHouse, Int)]
updateCounts house counts =
    let currentCount = countStudents house counts
    in if currentCount >= maxStudentsPerHouse
           then counts
           else (house, currentCount + 1) : filter (\(h, _) -> h /= house) counts

assignSchoolHouse :: [(SchoolHouse, Int)] -> IO (Maybe SchoolHouse)
assignSchoolHouse counts = do
    house <- pickRandom [Griffindor, Hufflepuff, Ravenclaw, Slytherin]
    if countStudents house counts >= maxStudentsPerHouse
        then return Nothing
        else return (Just house)

pickRandom :: [a] -> IO a
pickRandom xs = do
    idx <- randomRIO (0, length xs - 1)
    return (xs !! idx)

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y)
             where (x, y) = span (/= d) s

parseLine :: String -> Student
parseLine line =
    let [lastName, name] = split' ',' line
    in Student { lastName = lastName, name = name }

readFileToList :: FilePath -> IO [Student]
readFileToList filePath = do
    contents <- readFile filePath
    let students = map parseLine (lines contents)
    return students

main :: IO ()
main = do
    records <- readFileToList "/home/fundacion/Vídeos/LENGUAJESPROGRAMACION/REPOSITORIO-TAREAS/LenuajesProgramacion/list.txt"
    let studentsList [] _ = return ()
        studentsList (x:xs) counts = do
            maybeHouse <- assignSchoolHouse counts
            case maybeHouse of
                Just house -> do
                    let updatedCounts = updateCounts house counts
                    putStrLn $ show (x, house)
                    studentsList xs updatedCounts
                Nothing -> putStrLn $ "No hay espacio en ninguna casa para " ++ name x
    studentsList records []
