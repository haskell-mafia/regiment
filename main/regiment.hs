{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_ambiata_regiment
import           DependencyInfo_ambiata_regiment

import qualified Data.Attoparsec.Text as A
import           Data.Char (ord)
import           Data.Text as T
import           Data.Word (Word8)

import           Options.Applicative

import           P

import           Regiment.Data
import           Regiment.IO

import           System.IO
import           System.Exit

import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Data.Attoparsec.Text (positiveIntParser)
import           X.Options.Applicative (cli, Parser, subparser, command', strArgument)
import           X.Options.Applicative (metavar, help, maybeTextReader, option, auto, short, long, value, pOption)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  cli "regiment" buildInfoVersion dependencyInfo parser $ \cmd ->
    case cmd of
      SortCommand inn out sc sep m ->
        orDie renderSortError $
          sort inn out sc sep m

parser :: Parser Command
parser =
  subparser $
    command' "sort" "Sort input file based on sort column(s)."
      (SortCommand <$> inputFileP <*> outputDirectoryP <*> sortColumnP <*> separatorP <*> memP)

data Command =
  SortCommand InputFile OutputDirectory SortColumn Separator MemoryLimit
  deriving (Eq, Show)

inputFileP :: Parser InputFile
inputFileP =
  fmap InputFile . strArgument . mconcat $ [
      metavar "INPUT_FILE"
    , help "Path to input text file to shard."
    ]

outputDirectoryP :: Parser OutputDirectory
outputDirectoryP =
  fmap OutputDirectory . strArgument . mconcat $ [
      metavar "OUTPUT_DIRECTORY"
    , help "Path to output"
    ]

sortColumnP :: Parser SortColumn
sortColumnP =
  fmap SortColumn . option auto . mconcat $ [
      short 'k'
    , long "key"
    , help "Specify the sort column (1-indexed)."
    ]

separatorP :: Parser Separator
separatorP =
  fmap Separator . option (maybeTextReader toChar) . mconcat $ [
      short 'F'
    , long "field-separator"
    , help "Specify the field separator, defaults to '|'."
    , value pipe
    ]

toChar :: Text -> Maybe Word8
toChar t =
  case T.unpack t of
    [c] ->
      pure . fromIntegral . ord $ c
    [] ->
      fail "No separator provided."
    _ ->
      fail "Separator must be one character."

memP :: Parser MemoryLimit
memP =
  fmap MemoryLimit . option (pOption memParser) . mconcat $ [
      short 'm'
    , long "mem-max"
    , help "Specify the upper bound of memory to use - default unit is MB, use G to specify GB"
    ]

memParser :: A.Parser Int
memParser =
      positiveIntParser <* A.string "G"
  <|> positiveIntParser