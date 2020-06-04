{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( preprocess
    , splitYamls
    , getTemplatePath
    , helmCommand
    , generateStruct
    , processStructs
    , outputDir
    , Args(Args)
    )
where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.List
import           Data.List.Split
import           System.FilePath               as FP
import qualified Data.Maybe.Strict             as S
import           Turtle
import           Data.Maybe

-- |The command line arguments to use for the program
data Args = Args
    { chartName :: String
    , namespace :: Maybe String
    , valueFile :: Maybe String
    , outputDir :: Maybe String
    } deriving Show

-- |A struct containing the contents of a YAML file and its metadata
data Yaml = Yaml
    { yamlFilePath :: String -- File path of the YAML file
    , fileContents :: T.Text -- The text contents of the file
    } deriving (Show, Eq)

-- |The delimiter marking the difference between sections in combined YAML
-- files
yamlFileDelimiter :: T.Text
yamlFileDelimiter = "---"

-- | An arbitrary string to use for the name of the release. Helm requires some
-- release name, so we provide one. This also lets us strip the release name
-- out later.
helmReleaseName :: String
helmReleaseName = "generated"

-- |The Helm chart requests a release name that it adds to every name, we use
-- this identifier and delete it in the output files.
generatedReleasePrefix :: T.Text
generatedReleasePrefix = mconcat [T.pack helmReleaseName, "-"]

-- |Split a string that is comprised of multiple YAML files into separate YAML
-- files.
--
-- Helm provides YAML files that are structured as such:
--
-- ```yaml
-- ---
-- # YAML File 1
-- key: value
-- ---
-- # YAML File 2
-- key: value
-- ...
-- ```
--
-- This method splits the strings into a list of strings, where each string is
-- a separate yaml file.
splitYamls :: T.Text -> [T.Text]
splitYamls =
    map T.strip
        . filter ((<) 0 . T.length)
        . map T.unlines
        . filter ((<) 0 . length) -- remove empty chunks
        . splitOn [yamlFileDelimiter]
        . T.lines

-- |Preprocess the shell output from Helm into a YAML file that can be
-- interpreted by Kubernetes. This will strip debug fields, empty lines, and
-- trailing whitespace.
preprocess :: T.Text -> T.Text
preprocess =
    T.unlines
        . stripEmptyLines
        . withoutRelease
        . withoutEndNote
        . stripDebugFields
        . T.lines
  where
    stripEmptyLines  = filter (\x -> T.length x > 0)
    stripDebugFields = filter (not . isDebugField . head . T.splitOn ":")
    withoutEndNote   = takeWhile (\x -> T.strip x /= "NOTES:")
    withoutRelease   = map $ T.replace generatedReleasePrefix mempty

-- |Identifies whether a field is a debug field from Helm that isn't part of a
-- valid k8s spec
isDebugField :: T.Text -> Bool
isDebugField "NAME"          = True
isDebugField "LAST DEPLOYED" = True
isDebugField "NAMESPACE"     = True
isDebugField "STATUS"        = True
isDebugField "REVISION"      = True
isDebugField "TEST SUITE"    = True
isDebugField "HOOKS"         = True
isDebugField "MANIFEST"      = True
isDebugField _               = False

-- |A constant for the name of the `templates/` folder in Helm. This demarcates
-- the top-level directory for a set of rendered templates.
templateFolderName :: String
templateFolderName = "templates"

-- |Retrieve the name of the template that the chart. This is a fallible
-- operation, as there is no guarantee that the source comment will be present
-- in the YAML file. This also retrieves the directory of the given filepath,
-- getting the path from `/templates/`.
getTemplatePath :: T.Text -> Maybe FP.FilePath
getTemplatePath contents = do
    filepath <- getTemplatePath' contents
    let filename     = takeFileName filepath
    let tokenizedDir = FP.splitDirectories filepath
    let dir =
            (joinPath . tail . dropWhile (templateFolderName /=)) tokenizedDir
    return dir

sourceCommentLeader :: T.Text
sourceCommentLeader = "# Source:"

-- |Helper function to get the relative path of a template. This method
-- isolates the raw text demarcating the path.
getTemplatePath' :: T.Text -> Maybe FP.FilePath
getTemplatePath' contents = do
    let ls = T.lines contents
    sourceLine <- Data.List.find (T.isPrefixOf sourceCommentLeader) ls
    -- Given some string of the form "path/to/template.yaml", we take the last
    -- "/" chunk, and strip the ".yaml" from it to get the filename.
    filepath   <- T.unpack <$> T.stripPrefix sourceCommentLeader sourceLine
    return $ makeValid filepath

-- |Generate the Helm command to call to generate the YAML file
helmCommand :: Args -> String
helmCommand args =
    -- We *might* add the namespace and values command if they are provided
    unwords $ ["helm", "template", helmReleaseName, chartName args] ++ catMaybes
        [namespaceCmd, valuesCmd]
  where
    namespaceCmd = namespaceCommand $ namespace args
    valuesCmd    = valuesCommand $ valueFile args

-- |Generate the portion of the Helm command that dictates which namespace to
-- use
namespaceCommand :: Maybe String -> Maybe String
namespaceCommand = fmap $ addNamedFlag "--namespace"

-- |Generate the portion of the Helm command that dictates which values YAML
-- file to use
valuesCommand :: Maybe String -> Maybe String
valuesCommand = fmap $ addNamedFlag "-f"

-- |Given the name of a flag and the value of a flag, create a string with both
-- values in the proper order.
addNamedFlag :: String -> String -> String
addNamedFlag flagName flagValue = unwords [flagName, flagValue]

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
-- "0020"`. This is necessary so that the Helm chart files are listed in order.
indexFilePrefix :: Int -> Int -> String
indexFilePrefix x width = mconcat [filePrefix, xStr, "_"]
  where
    xStr             = show x
    nWidth           = length xStr
    numLeadingZeroes = max 0 width - nWidth
    filePrefix       = replicate numLeadingZeroes '0'

-- |Add a prefix to a filename, given the whole path. This will only modify the
-- base filename. This also handles converting the type to `Turtle.FilePath`.
addPrefixToPath :: String -> Turtle.FilePath -> Turtle.FilePath
addPrefixToPath prefix path = directoryOfPath <> prefixedFilename
  where
    directoryOfPath  = directory path
    prefixedFilename = mconcat [decodeString prefix, filename path]

-- |Process each YAML struct and save them to a file with the index in the
-- filename so all of the files are processed in the correct order (if they're
-- processed in alphabetical order).
processStructs :: Maybe String -> [Yaml] -> IO ()
processStructs out structs = do
    let charWidth = (length . show . length) structs
    let idxZipped = zip [0 ..] structs
    mapM_
        (\(index, yaml) -> do
            let prefix = indexFilePrefix index charWidth
            saveYamlFile prefix out yaml
        )
        idxZipped
