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
readName = undefined

-- [readName] asks the user to input a phone number and reads it from the standard input
readNumber :: IO String
readNumber = undefined

-- [readCommand] asks the user to input one of the available commads and reads it
readCommand :: IO Command
readCommand = do
  putStr "Choose command (add,search,remove,print,quit) : "
  undefined

-- [runOperation pb op] runs the operation [op] on the phone book [pb]
runOperation :: Phonebook -> Operation -> IO Phonebook
runOperation = undefined

-- [interactionLoop pb] reads a [Command] and performs the corresponding
-- action: an operation is executed and an unknown command is signalled to the
-- user by writing a warning. Until a [Quit] is encountered, at which point the
-- function returns.
interactionLoop :: Phonebook -> IO ()
interactionLoop = undefined

main :: IO ()
main = interactionLoop empty
