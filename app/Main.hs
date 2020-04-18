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
                    (long "values" <> short 'f' <> metavar "VALUES" <> help
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
helmCommand args =
    unwords $ ["helm", "template", "generated", chartName args] ++ catMaybes
        [ns, values]
  where
    ns     = namespaceCommand $ namespace args
    values = valuesCommand $ valueFile args

-- |Generate the portion of the Helm command that dictates which namespace to
-- use
namespaceCommand :: Maybe String -> Maybe String
namespaceCommand = fmap $ unwords . reverse . flip (:) ["--namespace"]

-- |Generate the portion of the Helm command that dictates which values YAML
-- file to use
valuesCommand :: Maybe String -> Maybe String
valuesCommand = fmap $ unwords . reverse . flip (:) ["-f"]

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
saveYamlFile :: String -> Maybe String -> Yaml -> IO ()
saveYamlFile indexPrefix parentDir f = do
    let rawPath = fullSavePath parentDir (yamlFilePath f)
    -- We rename the actual filename to add the index prefix, so "x.yaml" becomes "0001_x.yaml"
    let path    = addPrefixToPath indexPrefix rawPath
    TIO.putStrLn $ saveFileMessage path
    mktree $ (decodeString . takeDirectory . encodeString) path
    TIO.writeFile (encodeString path) (fileContents f)

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

-- |Pad a string representation of numbers with leading zeros `padZeros 4 20 ==
-- "0020"`. This is necessary so that the Helm chart files are evaluated in
-- order, since the Deployinator tool loads them in lexigraphical order.
indexFilePrefix :: Int -> Int -> String
indexFilePrefix n width = leadingZeros ++ strN ++ "_"
  where
    strN         = show n
    nWidth       = length strN
    leadingZeros = replicate (max (width - nWidth) 0) '0'

-- |Add a prefix to a filename, given the whole path. This will only modify the
-- base filename.
addPrefixToPath :: String -> Turtle.FilePath -> Turtle.FilePath
addPrefixToPath prefix path = dir <> decodeString newFileName
  where
    dir         = directory path
    newFileName = mconcat [prefix, encodeString (filename path)]

-- |Process each YAML struct and save them to a file with the index in the
-- filename so all of the files are processed in the correct order (if they're
-- processed in alphabetical order).
processStructs out structs = do
    let charWidth = (length . show . length) structs
    let idxZipped = zip [0 ..] structs
    mapM_
        (\(index, yaml) -> do
            let prefix = indexFilePrefix index charWidth
            saveYamlFile prefix out yaml
        )
        idxZipped

main :: IO ()
main = do
    args <- execParser opts
    let cmd = helmCommand args
    (code, out) <- shellStrict (T.pack cmd) empty
    if code == ExitSuccess
        then do
            putStrLn "Retrieved Helm output successfully"
            let preprocessed = preprocess out
            let structs      = (catMaybes . generateStruct) preprocessed
            processStructs (outputDir args) structs
            exitSuccess
        else do
            TIO.putStr out
            exitWith code
  where
    opts =
        info (optParser <**> helper)
            $  fullDesc
            <> progDesc "Render Helm charts"
            <> Options.Applicative.header "helm-renderer: render Helm charts"
