{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Lib
  ( preprocess,
    splitYamls,
    getTemplatePath,
    helmCommand,
    generateStruct,
    structsToFiles,
    outputDir,
    Args (Args),
  )
where

import qualified Data.HashSet as HashSet
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.FilePath as FP
import Turtle

-- | The command line arguments to use for the program
data Args = Args
  { chartName :: String,
    namespace :: Maybe String,
    valueFile :: Maybe String,
    outputDir :: Maybe String
  }
  deriving (Show)

-- | A struct containing the contents of a YAML file and its metadata
data Yaml = Yaml
  { -- | File path of the YAML file
    yamlFilePath :: String,
    -- | The text contents of the file
    fileContents :: T.Text
  }
  deriving (Show, Eq)

-- | The delimiter marking the difference between sections in combined YAML
--  files
yamlFileDelimiter :: T.Text
yamlFileDelimiter = "---"

-- | An arbitrary string to use for the name of the release. Helm requires some
-- release name, so we provide one. This also lets us strip the release name
-- out later.
helmReleaseName :: String
helmReleaseName = "generated"

-- | The Helm chart requests a release name that it adds to every name, we use
--  this identifier and delete it in the output files.
generatedReleasePrefix :: T.Text
generatedReleasePrefix = T.pack helmReleaseName <> "-"

-- | A set of debug fields fields that Helm outputs in its rendered charts with
-- debug output
debugFields :: HashSet.HashSet T.Text
debugFields =
  HashSet.fromList
    [ "NAME",
      "LAST DEPLOYED",
      "NAMESPACE",
      "STATUS",
      "REVISION",
      "TEST SUITE",
      "HOOKS",
      "MANIFEST"
    ]

-- | Identifies whether a field is a debug field from Helm
--
-- We use this method to strip out fields that aren't part of the Kubernetes
-- spec.
isDebugField :: T.Text -> Bool
isDebugField = flip HashSet.member debugFields

-- | The prefix in a rendered helm YAML file that indicates which template file
-- a section was rendered from
sourceCommentLeader :: T.Text
sourceCommentLeader = "# Source:"

-- | Split a string that is comprised of multiple YAML files into separate YAML
--  files.
--
--  Helm provides YAML files that are structured as such:
--
--  ```yaml
--  ---
--  # YAML File 1
--  key: value
--  ---
--  # YAML File 2
--  key: value
--  ...
--  ```
--
--  This method splits the strings into a list of strings, where each string is
--  a separate yaml file.
splitYamls :: T.Text -> [T.Text]
splitYamls =
  map T.strip
    . filter T.null
    . map T.unlines
    . filter null -- remove empty chunks
    . splitOn [yamlFileDelimiter]
    . T.lines

-- | Preprocess the shell output from Helm into a YAML file that can be
--  interpreted by Kubernetes. This will strip debug fields, empty lines, and
--  trailing whitespace.
preprocess :: T.Text -> T.Text
preprocess =
  T.unlines
    . stripEmptyLines
    . withoutRelease
    . withoutEndNote
    . stripDebugFields
    . T.lines
  where
    stripEmptyLines = filter (not . T.null)
    withoutEndNote = takeWhile (\x -> T.strip x /= "NOTES:")
    withoutRelease = map $ T.replace generatedReleasePrefix mempty

-- | Strip debug fields from a yaml file
--
-- We define a "debug fields" as a key-value pair in a YAML file whose key
-- returns true when applied to `isDebugField`.
stripDebugFields :: [T.Text] -> [T.Text]
-- Need a safe version for an empty list, since we're using `head` in the other
-- branch
stripDebugFields [] = []
stripDebugFields xs =
  filter
    (not . isDebugField . extractHead . safeHead . T.splitOn ":")
    xs
  where
    extractHead :: Maybe T.Text -> T.Text
    extractHead = Data.Maybe.fromMaybe ""

-- | A safe replacement for `head`
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- | A constant for the name of the `templates/` folder in Helm. This demarcates
--  the top-level directory for a set of rendered templates.
templateFolderName :: String
templateFolderName = "templates"

-- | Retrieve the name of the template that the chart. This is a fallible
--  operation, as there is no guarantee that the source comment will be present
--  in the YAML file. This also retrieves the directory of the given filepath,
--  getting the path from `/templates/`.
getTemplatePath :: T.Text -> Maybe FP.FilePath
getTemplatePath contents = do
  filepath <- getTemplatePath' contents
  let tokenizedDir = FP.splitDirectories filepath
  let dir =
        (joinPath . tail . dropWhile (templateFolderName /=)) tokenizedDir
  return dir

-- | Helper function to get the relative path of a template. This method
--  isolates the raw text demarcating the path.
getTemplatePath' :: T.Text -> Maybe FP.FilePath
getTemplatePath' contents = do
  let ls = T.lines contents
  sourceLine <- Data.List.find (T.isPrefixOf sourceCommentLeader) ls
  -- Given some string of the form "path/to/template.yaml", we take the last
  -- "/" chunk, and strip the ".yaml" from it to get the filename.
  filepath <- T.unpack <$> T.stripPrefix sourceCommentLeader sourceLine
  return $ makeValid filepath

-- | Generate the Helm command to call to generate the YAML file
helmCommand ::
  -- | Arguments to provide to helm
  Args ->
  -- | A generated Helm command invocation
  String
helmCommand args =
  -- We *might* add the namespace and values command if they are provided
  unwords $
    ["helm", "template", helmReleaseName, chartName args]
      ++ catMaybes
        [namespaceCmd, valuesCmd]
  where
    namespaceCmd = namespaceCommand <$> namespace args
    valuesCmd = valuesCommand <$> valueFile args

-- | Generate the portion of the Helm command that dictates which namespace to
--  use
namespaceCommand :: String -> String
namespaceCommand = addNamedFlag "--namespace"

-- | Generate the portion of the Helm command that dictates which values YAML
--  file to use
valuesCommand :: String -> String
valuesCommand = addNamedFlag "-f"

-- | Given the name of a flag and the value of a flag, create a string with both
--  values in the proper order.
addNamedFlag :: String -> String -> String
addNamedFlag flagName flagValue = unwords [flagName, flagValue]

-- | Create YAML struct objects with metadata from the Helm output
generateStruct :: Text -> [Maybe Yaml]
generateStruct txt = map toStruct split
  where
    split = splitYamls txt
    toStruct :: T.Text -> Maybe Yaml
    toStruct x = do
      fp <- getTemplatePath x
      return (Yaml fp x)

-- | Write the yaml file, given its contents, path, and the parent path
saveYamlFile :: String -> Maybe String -> Yaml -> IO ()
saveYamlFile indexPrefix parentDir f = do
  let rawPath = fullSavePath parentDir (yamlFilePath f)
  -- We rename the actual filename to add the index prefix, so "x.yaml" becomes "0001_x.yaml"
  let path = addPrefixToPath indexPrefix rawPath
  TIO.putStrLn $ saveFileMessage path
  mktree $ (decodeString . takeDirectory . encodeString) path
  TIO.writeFile (encodeString path) (fileContents f)

-- | Create a message to display to the user informing them that a file has been
--  saved. This will print an error message if the filepath can't be easily
--  converted to a human-readable format.
saveFileMessage :: Turtle.FilePath -> T.Text
saveFileMessage = f . toText
  where
    f :: Either T.Text T.Text -> T.Text
    f (Left e) =
      "A file was saved but the filename could not be printed."
        <> "Error: "
        <> e
    f (Right text) = "Saved " <> text

-- | Calculate the full save path of a YAML file given the config
fullSavePath ::
  -- | The optional directory to save the YAML file in
  Maybe String ->
  -- | The file path to save the YAML file
  String ->
  -- | The full save path for the YAML file
  Turtle.FilePath
fullSavePath parent fp = mconcat $ decodeString <$> paths
  where
    paths = catMaybes [parent, pure fp]

-- | Pad a string representation of numbers with leading zeros, given the total
-- width available.
--
-- We use this so that the Helm chart files are listed in order when you sort
-- filenames alphabetically, like when you use `ls. Note that if you supply a
-- width that is less than the number of digits in `x`, this will simply return
-- `x` as a string and will *not* truncate the number.
indexFilePrefix ::
  -- | The number to pad
  Int ->
  -- | The total width to pad to
  Int ->
  -- | The number as a string that's padded with leading zeros
  String
indexFilePrefix number width = leftPad <> numberStr <> "_"
  where
    numberStr = show number
    nWidth = length numberStr
    numLeadingZeroes = max 0 (width - nWidth)
    leftPad = replicate numLeadingZeroes '0'

-- | Add a prefix to a filename, given the whole path. This will only modify the
--  base filename. This also handles converting the type to `Turtle.FilePath`.
addPrefixToPath ::
  -- | The prefix to add to the filename
  String ->
  -- | The template path
  Turtle.FilePath ->
  -- | The resulting filepath with the prepended prefix
  Turtle.FilePath
addPrefixToPath prefix path = directoryOfPath <> prefixedFilename
  where
    directoryOfPath = directory path
    prefixedFilename = decodeString prefix <> filename path

-- | Process each YAML struct and save them to a file with the index in the
--  filename so all of the files are processed in the correct order (if they're
--  processed in alphabetical order).
structsToFiles :: Maybe String -> [Yaml] -> IO ()
structsToFiles outputDirectory structs = do
  let paddedWidth = (length . show . length) structs
  let idxZipped = zip [0 ..] structs
  mapM_
    ( \(index, yaml) -> do
        let prefix = indexFilePrefix index paddedWidth
        saveYamlFile prefix outputDirectory yaml
    )
    idxZipped
