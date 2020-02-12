{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( preprocess
    , splitYamls
    , getTemplatePath
    )
where

import qualified Data.Text                     as T
import           Data.List
import           System.FilePath
import qualified Data.Maybe.Strict             as S

-- |The delimiter marking the difference between sections in combined YAML
-- files 
yamlFileDelimiter :: T.Text
yamlFileDelimiter = "---"

-- |The Helm chart requests a release name that it adds to every name, we use
-- this identifier and delete it in the output files.
generatedReleasePrefix :: T.Text
generatedReleasePrefix = "generated-"

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
    filter (\x -> T.length x > 0) . map T.strip . T.splitOn yamlFileDelimiter

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
--
-- This returns
getTemplatePath :: T.Text -> Maybe FilePath
getTemplatePath contents = do
    filepath <- getTemplatePath' contents
    let filename     = takeFileName filepath
    let tokenizedDir = splitDirectories filepath
    let dir =
            (joinPath . tail . dropWhile (templateFolderName /=)) tokenizedDir
    return dir

sourceCommentLeader :: T.Text
sourceCommentLeader = "# Source:"

-- |Helper function to get the relative path of a template. This method
-- isolates the raw text demarcating the path.
getTemplatePath' :: T.Text -> Maybe FilePath
getTemplatePath' contents = do
    let ls = T.lines contents
    sourceLine <- find (T.isPrefixOf sourceCommentLeader) ls
    -- Given some string of the form "path/to/template.yaml", we take the last
    -- "/" chunk, and strip the ".yaml" from it to get the filename.
    filepath   <- T.unpack <$> T.stripPrefix sourceCommentLeader sourceLine
    return $ makeValid filepath
