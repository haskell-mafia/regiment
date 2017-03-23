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

import qualified System.IO as IO
import           System.IO (IO)

import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Data.Attoparsec.Text (positiveIntParser)
import           X.Options.Applicative (cli, command')
import           X.Options.Applicative (maybeTextReader, pOption)

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.LineBuffering
  IO.hSetBuffering IO.stderr IO.LineBuffering
  cli "regiment" buildInfoVersion dependencyInfo parser $ \cmd ->
    case cmd of
      SortCommand inn out nc sc sep m f n ->
        orDie renderRegimentIOError $
          regiment inn out sc f n nc sep m

parser :: Parser Command
parser =
  subparser $
    command' "sort" "Sort input file based on sort column(s)."
      (SortCommand <$> inputFileP
                   <*> optional outputP
                   <*> numColumnsP
                   <*> some sortColumnP
                   <*> separatorP
                   <*> memP
                   <*> formatP
                   <*> newlineP)

data Command =
  SortCommand InputFile (Maybe OutputFile) NumColumns [SortColumn] Separator MemoryLimit FormatKind Newline
  deriving (Eq, Show)

inputFileP :: Parser InputFile
inputFileP =
  fmap InputFile . strArgument . mconcat $ [
      metavar "INPUT_FILE"
    , help "Path to input text file to regiment."
    ]

outputP :: Parser OutputFile
outputP = OutputFile <$> (strOption $
     long "output"
  <> short 'o'
  <> metavar "FILE"
  <> help "Optional path to output file -- defaults to stdout.")

sortColumnP :: Parser SortColumn
sortColumnP =
  fmap (SortColumn . (\k -> k - 1)) . option auto . mconcat $ [
      short 'k'
    , long "key"
    , help "Specify the sort column (1-indexed)."
    ]

separatorP :: Parser Separator
separatorP =
  fmap Separator . option (maybeTextReader toChar) . mconcat $ [
      short 'f'
    , long "field-separator"
    , help "Specify the field separator"
    ]

memP :: Parser MemoryLimit
memP =
  fmap MemoryLimit . option (pOption memParser) . mconcat $ [
      short 'm'
    , long "mem-max"
    , help "Specify the upper bound of memory to use - default unit is MB, use G to specify GB"
    , value (2048 * 1024)
    ]

memParser :: A.Parser Int
memParser =
      positiveIntParser <* A.string "G"
  <|> positiveIntParser

numColumnsP :: Parser NumColumns
numColumnsP =
  fmap NumColumns . option auto . mconcat $ [
      short 'c'
    , long "number-columns"
    , help "Number of columns in the file."
    ]

formatP :: Parser FormatKind
formatP =
  fromMaybe Delimited <$> optional (delimitedP <|> standardizedP)

delimitedP :: Parser FormatKind
delimitedP =
  flag' Delimited . mconcat $ [
      long "delimited"
    , help "The format of the file is delimited (default)."
    ]

standardizedP :: Parser FormatKind
standardizedP =
  flag' Standardized . mconcat $ [
      long "standardized"
    , help "The format of the file is standardized."
    ]

newlineP :: Parser Newline
newlineP =
  fromMaybe LF <$> optional (lfP <|> crP <|> crlfP)

lfP :: Parser Newline
lfP =
  flag' LF . mconcat $ [
      long "lf"
    , help "The input file uses \\n to terminate lines (default)."
    ]

crP :: Parser Newline
crP =
  flag' CR . mconcat $ [
      long "cr"
    , help "The input file uses \\r to terminate lines."
    ]

crlfP :: Parser Newline
crlfP =
  flag' CRLF . mconcat $ [
      long "crlf"
    , help "The input file uses \\r\\n to terminate lines."
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

