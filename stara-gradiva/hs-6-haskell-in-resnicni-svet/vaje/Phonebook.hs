import Dict

-- a phone book maps names to numbers. But phone numbers can be messy, so we
-- use [String] to represent them.
type Phonebook = Dict String String

-- we make some basic operations on phone books available to the user
data Operation = Add | Search | Remove | PrintAll

-- a command tells us to either perform an operation on a phone book or to stop
data Command = Op Operation | UnknownCmd String | Quit

-- [readName] asks the user to input a name and reads it from the standard input
readName :: IO String
readName = do
  putStr "Enter name: "
  getLine

-- [readName] asks the user to input a phone number and reads it from the standard input
readNumber :: IO String
readNumber = do
  putStr "Enter number: "
  getLine

-- [readCommand] asks the user to input one of the available commads and reads it
readCommand :: IO Command
readCommand = do
  putStr "Choose command (add,search,remove,print,quit) : "
  cmd <- getLine
  return $ case cmd of
    "add" -> Op Add
    "search" -> Op Search
    "remove" -> Op Remove
    "print" -> Op PrintAll
    "quit" -> Quit
    _ -> UnknownCmd cmd

-- [runOperation pb op] runs the operation [op] on the phone book [pb]
runOperation :: Phonebook -> Operation -> IO Phonebook
runOperation pb Search = do name <- readName
                            case search name pb of
                              Nothing -> putStrLn $ "Name \"" ++ name ++ "\" not found."
                              Just nb -> putStrLn $ "Found number: " ++ nb
                            return pb
runOperation pb Add = do name <- readName
                         num <- readNumber
                         return $ add name num pb
runOperation pb Remove = do name <- readName
                            return $ remove name pb
runOperation pb PrintAll = do putStrLn $ show pb
                              return pb

-- [interactionLoop pb] reads a [Command] and performs the corresponding
-- action: an operation is executed and an unknown command is signalled to the
-- user by writing a warning. Until a [Quit] is encountered, at which point the
-- function returns.
interactionLoop :: Phonebook -> IO ()
interactionLoop pb = do
  step <- readCommand
  case step of
    UnknownCmd cmd -> do
      putStrLn $ "Unknown command " ++ cmd
      interactionLoop pb
    Quit -> return ()
    Op op -> do
      pb' <- runOperation pb op
      interactionLoop pb'

main :: IO ()
main = interactionLoop empty
