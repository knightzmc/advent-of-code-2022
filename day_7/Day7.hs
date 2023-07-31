import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT, evalState, execState, get, modify, put, runState)
import Data.Functor
import Data.Map qualified as M
import Data.Maybe
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char (alphaNumChar, char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Debug

data AOCState = AOCState
    { currentDirectory :: [String]
    , root :: Directories
    }

type Directories = M.Map String DirectoryEntry

data DirectoryEntry
    = Directory String Directories
    | File String Int

subfiles :: DirectoryEntry -> Directories
subfiles (File _ _) = error "file"
subfiles (Directory _ s) = s

data Command
    = CD String
    | LS [(Int, String)]
    deriving (Show)

type AOCMonad = Parsec Void String

anything :: AOCMonad String
anything = many (alphaNumChar <|> char '.' <|> char '/') <?> "anything"

instruction :: AOCMonad Command
instruction = (cdInstruction <|> lsInstruction) <?> "instruction"
  where
    cdInstruction :: AOCMonad Command
    cdInstruction =
        ( do
            string "$ cd "
            CD <$> anything
        )
            <?> "cdInstruction"
    lsInstruction =
        ( do
            string "$ ls\n"
            entries <- dbg "entries" $ manyTill (entry <* newline) (char '$')
            pure (LS (catMaybes entries))
        )
            <?> "lsInstruction"

entry :: AOCMonad (Maybe (Int, String))
entry = (fileEntry <|> dirEntry) <?> "entry"

dirEntry :: AOCMonad (Maybe (Int, String))
dirEntry = ((string "dir " *> anything) $> Nothing) <?> "dirEntry"

fileEntry :: AOCMonad (Maybe (Int, String))
fileEntry =
    ( do
        size <- decimal
        char ' '
        name <- anything
        pure (Just (size, name))
    )
        <?> "fileEntry"

main :: IO ()
main = do
    c <- readFile "input.txt"
    let x = runParser (sepBy instruction newline <* eof) "" c
    case x of
        Left err -> putStr (errorBundlePretty err)
        Right x -> print x