module Main where
import System.Random

main :: IO ()

main = do
    putStrLn "Hello, welcome to bangman!"
    file <- readFile "words.txt"
    let wordList = words file

    putStrLn "Guess the word!"

    n <- randomRIO(0, length wordList -1)
    let secret = "imperial"

    turn 4 secret [] []
    return ()


type Secret = String

turn :: Int -> Secret -> [Char] -> [Char] -> IO()
turn 0 secret wrong correct = do
    putStrLn "Game over bitch"
    putStrLn "The word was:"
    print secret

turn n secret wrong correct
    | all (`elem` correct) secret = do
        putStrLn "Well done! The word was:"
        putStrLn secret
    | otherwise = do
        putStrLn "Length of secret:"
        print (length secret)
        putStrLn "Wrong letters:"
        putStrLn wrong
        putStrLn "Correct letters:"
        putStrLn correct
        putStrLn "What is your guess?"
        c <- getLetter

        if c `elem` secret
            then do
                putStrLn "Correct!"
                turn n secret wrong (c:correct)
            else do
                putStrLn "Incorrect"
                turn (n-1) secret (c:wrong) correct
        


getLetter :: IO Char
getLetter = do
    putStrLn "Please guess a character"
    cs <- getLine
    case cs of
        [c] -> return c
        _   -> getLetter