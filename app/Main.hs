{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib
import Options.Applicative
import System.Exit
import System.FilePath
import Turtle

optParser :: Parser Args
optParser =
  Args
    <$> strArgument
      ( metavar "CHARTNAME"
          <> help
            "The name of the chart in the form 'repo/chartname'"
      )
    <*> optional
      ( strOption
          ( long "namespace"
              <> short 'n'
              <> metavar "NAMESPACE"
              <> help "The namespace to pass to Helm"
          )
      )
    <*> optional
      ( strOption
          ( long "values" <> short 'f' <> metavar "VALUES"
              <> help
                "The path to the optional values YAML"
          )
      )
    <*> optional
      ( strOption
          ( long "out" <> metavar "OUTDIR"
              <> help
                "The output directory to write the generated files to"
          )
      )

-- | Process the output from a helm command, given the output from the helm
-- process and the arguments used to generate the output.
processHelmOutput :: Text -> Args -> IO ()
processHelmOutput output args = do
  let structs = (catMaybes . generateStruct . preprocess) output
  structsToFiles (outputDir args) structs

main :: IO ()
main = do
  args <- execParser opts
  let cmd = helmCommand args
  (code, out) <- shellStrict (T.pack cmd) empty
  if code == ExitSuccess
    then do
      putStrLn "Retrieved Helm output successfully"
      processHelmOutput out args
    else do
      TIO.putStr out
  exitWith code
  where
    opts =
      info (optParser <**> helper) $
        fullDesc
          <> progDesc "Render Helm charts"
          <> Options.Applicative.header "helm-renderer"
