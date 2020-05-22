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
            <> Options.Applicative.header "helm-renderer"
