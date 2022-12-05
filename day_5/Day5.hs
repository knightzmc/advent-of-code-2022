import Control.Monad (replicateM)
import Control.Monad.Trans.State
import Data.Foldable (traverse_)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Void (Void)
import Debug.Trace (traceShowM)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, count, errorBundlePretty, many, manyTill, optional, parse, sepBy, sepBy1, (<|>))
import Text.Megaparsec.Char (char, newline, string, upperChar)
import Text.Megaparsec.Char.Lexer qualified as L

type Stacks =
    IntMap (Stack Char)

data Stack a
    = Empty
    | Snoc (Stack a) a

instance Show a => Show (Stack a) where
    show = show . toList

sHead :: Stack a -> a
sHead Empty = error "Empty stack"
sHead (Snoc f a) = a

popStack :: Stack a -> (a, Stack a)
popStack Empty = error "Empty stack"
popStack (Snoc f a) = (a, f)

toStack :: [a] -> Stack a
toStack [] = Empty
toStack (a : xs) = Snoc (toStack xs) a

toList :: Stack a -> [a]
toList Empty = []
toList (Snoc f a) = a : toList f

pop :: Int -> State Stacks Char
pop i = do
    state <- get
    let toPop = state IntMap.! i
    let (res, newStack) = popStack toPop
    let newStacks = IntMap.insert i newStack state
    put newStacks
    pure res

sTake :: Int -> Int -> State Stacks [Char]
sTake i count = do
    chars <- replicateM count (pop i)
    pure (reverse chars)

append :: Int -> Char -> State Stacks ()
append i c = do
    state <- get
    let toAppend = state IntMap.! i
    let newStack = Snoc toAppend c
    let newStacks = IntMap.insert i newStack state
    put newStacks

data Instruction = Move Int Int Int deriving (Show)

runInstruction :: Instruction -> State Stacks ()
runInstruction (Move 0 from to) = pure ()
runInstruction (Move count from to) = do
    toMove <- pop from
    append to toMove
    runInstruction (Move (count - 1) from to)

runInstructionP2 :: Instruction -> State Stacks ()
runInstructionP2 i@(Move count from to) = do
    toMove <- sTake from count
    traverse_ (append to) toMove

-- Parsing stage
type Parser = Parsec Void String

block :: Parser (Maybe Char)
block = realBlock <|> blank
  where
    realBlock = do
        char '['
        c <- upperChar
        char ']'
        pure (Just c)
    blank = do
        count 3 (char ' ')
        pure Nothing

blocks = sepBy1 block (char ' ')

move :: Parser Instruction
move = do
    string "move "
    count <- L.decimal
    string " from "
    from <- L.decimal
    string " to "
    Move count from <$> L.decimal

blocksToStacks :: [[Maybe Char]] -> Stacks
blocksToStacks grid =
    let x = transpose grid
     in let stacks = toStack . catMaybes <$> x
         in IntMap.fromAscList (zip [1 ..] stacks)

-- blocks

-- file :: Parser (Stacks, [Instruction])
file = do
    stacks <- manyTill (blocks <* newline) (try $ string " 1")
    let x = blocksToStacks stacks
    manyTill anySingle newline -- numbers line
    manyTill anySingle newline -- blank line
    insts <- sepBy1 move newline
    pure (x, insts)

main = do
    c <- readFile "input.txt"
    case parse file "" c of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right (stacks, instructions) -> do
            let !endState = execState (traverse runInstruction instructions) stacks
             in putStrLn $ sHead <$> IntMap.elems endState

            -- part 2
            let !endState2 = execState (traverse runInstructionP2 instructions) stacks
             in putStrLn $ sHead <$> IntMap.elems endState2