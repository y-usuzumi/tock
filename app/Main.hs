import           Data.List.Split
import           Debug.Trace
import           Options.Applicative
import           System.Directory
import           System.FilePath.Posix
import           Tock.CodeGen.Markdown
import           Tock.DataTypes

data Args = Args { dir              :: FilePath
                 , excludeEmptyDirs :: Bool
                 , fileExtensions   :: Maybe [String]
                 }


argsParser :: Parser Args
argsParser = Args
  <$> argument str ( metavar "dir"
                   <> help "The root directory to look for contents"
                   )
  <*> flag False True (short 'x'
                      <> help "Exclude directories that do not have files (recursively)"
                      )
  <*> do
    s <- strOption ( long "file-extensions"
                   <> short 'e'
                   <> metavar "FILE-EXTENSIONS"
                   <> help "Comma-separated file extensions to include"
                   <> value ""
                   )
    return $ if null s then Nothing else Just (splitOn "," s)

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
  ( fullDesc
  <> progDesc "Generate markdown of TOC"
  <> header "tock"
  )

run :: Args -> IO ()
run Args{..} = genToc genOptions dir >>= putStrLn . renderTocs renderOptions
  where
    renderOptions = RenderOptions { excludeEmptyDirs = excludeEmptyDirs}
    genOptions = GenOptions { fileExtensions = fileExtensions }


main :: IO ()
main = execParser opts >>= run
