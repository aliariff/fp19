module S3Ex3 where
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
  return ()
  -- end replace

library :: [(String, String)] -> IO ()
library books = do
  -- task c)
  -- replace with implementation:
  item <- getInput
  process item

 where
  process :: LibraryInput -> IO ()
  process Exit = return ()

  process (Error xs) =
    putStrLn ("There has been an error: " ++ xs) >> library books

  process (Book (title, author)) = do
    putStrLn
      "Do you want to (p)ut the book back or do you want to (t)ake the book?"
    choice <- getLine
    processBook choice (Book (title, author))

  process (Author author) = printBooks
    "author"
    author
    ("You have no books from " ++ author)
    ("You have the following books from " ++ author)

  process (Title title) = printBooks
    "title"
    title
    ("You have no books with the title: " ++ title)
    ("You have the following books with the title: " ++ title)

  processBook :: String -> LibraryInput -> IO ()
  processBook "p" (Book (title, author)) =
    putStrLn "Done!" >> library (books ++ [(title, author)])
  processBook "t" (Book (title, author)) = if (title, author) `elem` books
    then putStrLn "Done!"
      >> library (filter (\(t, a) -> t /= title || a /= author) books)
    else putStrLn "You do not have this book!" >> library books
  processBook _ _ = putStrLn "Wrong input!" >> library books

  printBooks :: String -> String -> String -> String -> IO ()
  printBooks attribute query errorMsg successMsg
    | filteredBooks == [] = putStrLn errorMsg >> library books
    | otherwise = putStrLn successMsg >> putStrLn joinedBooks >> library books
   where
    filteredBooks = if attribute == "author"
      then filter (\(_, a) -> a == query) books
      else filter (\(t, _) -> t == query) books

    bookInstances = map (\(t, a) -> Book (t, a)) filteredBooks

    joinedBooks =
      foldr ((\str xs -> xs ++ str ++ "\n") . show) "" bookInstances

  -- end replace

getInput :: IO LibraryInput
getInput = do
  -- task b)
  putStrLn
    "Would you like to put back or take a book?\n Enter Book: Title's name; Author's name \nAre you looking for an author?\n Enter Author: Author's name \nAre you looking for a special book?\n Enter Title: Title's name."
    -- task b)
  -- replace with implementation:
  putStrLn ">"
  input <- getLine
  return (parseLibraryInput input)
  -- end replace
