{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Lib
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )
import           System.FilePath
import           Turtle
import           Data.Maybe
import           System.Exit

-- |The command line arguments to use for the program
data Args = Args
    { chartName :: String
    , namespace :: Maybe String
    , valueFile :: Maybe String
    , outputDir :: Maybe String
    } deriving Show

-- |A struct containing the contents of a YAML file and its metadata
data Yaml = Yaml
    { yamlFilePath :: String
    , fileContents :: T.Text
    } deriving Show

optParser :: Parser Args
optParser =
    Args
        <$> strArgument
                (metavar "CHARTNAME" <> help
                    "The name of the chart in the form 'repo/chartname'"
                )
        <*> optional
                (strOption
                    (  long "namespace"
                    <> short 'n'
                    <> metavar "NAMESPACE"
                    <> help "The namespace to pass to Helm"
                    )
                )
        <*> optional
                (strOption
                    (long "values" <> metavar "VALUES" <> help
                        "The path to the optional values YAML"
                    )
                )
        <*> optional
                (strOption
                    (long "out" <> metavar "OUTDIR" <> help
                        "The output directory to write the generated files to"
                    )
                )

-- |Generate the Helm command to call to generate the YAML file
helmCommand :: Args -> String
helmCommand args = unwords
    ["helm", "install", "generated", chartName args, "--dry-run", ns, values]
  where
    ns     = fromMaybe "" $ namespaceCommand $ namespace args
    values = fromMaybe "" $ valuesCommand $ valueFile args

-- |Generate the portion of the Helm command that dictates which namespace to
-- use
namespaceCommand :: Maybe String -> Maybe String
namespaceCommand Nothing   = Nothing
namespaceCommand (Just ns) = pure $ unwords ["--namespace", ns]

-- |Generate the portion of the Helm command that dictates which values YAML
-- file to use
valuesCommand :: Maybe String -> Maybe String
valuesCommand Nothing       = Nothing
valuesCommand (Just values) = pure $ unwords ["-f", values]

-- |Create YAML struct objects with metadata from the Helm output
generateStruct :: Text -> [Maybe Yaml]
generateStruct txt = map toStruct split
  where
    split = splitYamls txt
    toStruct :: T.Text -> Maybe Yaml
    toStruct x = do
        fp <- getTemplatePath x
        return (Yaml fp x)

-- |Write the yaml file, given its contents, path, and the parent path
saveYamlFile :: Maybe String -> Yaml -> IO ()
saveYamlFile parentDir f = do
    let fullPath = fullSavePath parentDir (yamlFilePath f)
    TIO.putStrLn $ saveFileMessage fullPath
    mktree $ (decodeString . takeDirectory . encodeString) fullPath
    TIO.writeFile (encodeString fullPath) (fileContents f)

-- |Create a message to display to the user informing them that a file has been
-- saved. This will print an error message if the filepath can't be easily
-- converted to a human-readable format.
saveFileMessage :: Turtle.FilePath -> T.Text
saveFileMessage = f . toText
  where
    f :: Either T.Text T.Text -> T.Text
    f (Left  _   ) = "A file was saved but the filename could not be printed"
    f (Right text) = mconcat ["Saved ", text]

-- |Calculate the full save path of a YAML file given the config
fullSavePath :: Maybe String -> String -> Turtle.FilePath
fullSavePath Nothing       fp = decodeString fp
fullSavePath (Just parent) fp = decodeString parent <> decodeString fp

main :: IO ()
main = do
    args <- execParser opts
    let cmd = helmCommand args
    (code, out) <- shellStrict (T.pack cmd) empty
    if code /= ExitSuccess
        then do
            TIO.putStr out
            exitWith code
        else do
            putStrLn "Retrieved Helm output successfully"
            let preprocessed = preprocess out
            let structs      = (catMaybes . generateStruct) out
            mapM_ (saveYamlFile (outputDir args)) structs
            exitSuccess
  where
    opts =
        info (optParser <**> helper)
            $  fullDesc
            <> progDesc "Render Helm charts"
            <> Options.Applicative.header "helm-renderer: render Helm charts"
