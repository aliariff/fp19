module S3Ex3Alt where

import           Data.Char

data LibraryInput = Exit | Error String | Book (String, String) | Author String | Title String

instance Show LibraryInput where
  show Exit                     = "Exit"
  show (Error  xs             ) = "Invalid Input: " ++ xs
  show (Book   (title, author)) = "Book: " ++ title ++ ";" ++ author
  show (Author author         ) = "Author: " ++ author
  show (Title  title          ) = "Title: " ++ title

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

parseLibraryInput :: String -> LibraryInput
parseLibraryInput input
  | lhs == "Book"   = Book ((trim (drop 1 title)), (trim (drop 1 author)))
  | lhs == "Author" = Author (trim (drop 1 rhs))
  | lhs == "Title"  = Title (trim (drop 1 rhs))
  | elem (map toLower input) ["q", "e", "exit", "quit"] = Exit
  | otherwise       = (Error input)
 where
  (lhs  , rhs   ) = span (/= ':') (trim input)
  (title, author) = span (/= ';') (trim rhs)



-- exercise:
main :: IO ()
main = do
  -- task a)
  -- replace with implementation:
  putStrLn "Welcome to your Library"
  library []
  putStrLn "Bye!"
  -- end replace

library :: [(String, String)] -> IO ()
library books = do
  -- task c)
  input <- getInput
  case input of
    Exit    -> return ()
    Error q -> do
      putStrLn ("There has been an error:" ++ q)
      library books
    Book b -> do
      putStrLn
        "Do you want to (p)ut the book back or do you want to (t)ake the book?"
      choice <- getLine
      if elem (map toLower choice) ["p", "t"]
        then do
          case choice of
            "p" -> do
              putStrLn "Done!"
              library (b : books)
            "t" -> do
              if elem b books
                then do
                  putStrLn "Done!"
                  library ([ x | x <- books, x /= b ])
                else do
                  putStrLn "You do not have this book!"
                  library books
        else do
          putStrLn "Wrong input!"
          library books


    Author a -> do
      putStrLn ("You have the following books from " ++ a)
      print ([ fst x | x <- books, snd (x) == a ])
      library books
    Title t -> do
      putStrLn ("You have the following books with the title: " ++ t)
      print ([ fst x | x <- books, fst (x) == t ])
      library books



  -- end replace


getInput :: IO LibraryInput
getInput = do
  -- task b)
  putStrLn
    "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
-- task b)
  putStrLn ">"
  input <- getLine
  return (parseLibraryInput input)

  -- end replace
