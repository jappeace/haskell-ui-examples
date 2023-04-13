{-# LANGUAGE BangPatterns #-}
module UI.OptParse
  ( main
  )
where

import Options.Applicative
import qualified UI.Interact
import Data.Foldable(fold)
import qualified UI.Yesod
import qualified UI.Gtk
import qualified UI.Sdl

data Program = CLI OptParseOptions
             | Interact
             | Yesod
             | Gtk
             | Sdl


cliOptions :: Parser Program
cliOptions = CLI <$> cliParser

programParser :: Parser Program
programParser = subparser $ fold
      [ command "cli" (info cliOptions (progDesc "Greet the user, cli trough opt parse applicative"))
      , command "yesod" (info (pure Yesod) (progDesc "Start yesod server, a web framework"))
      , command "interact" (info (pure Interact) (progDesc "interact, the most basic of cli"))
      , command "gtk" (info (pure Gtk) (progDesc "gi-gtk gimp tool kit"))
      , command "sdl" (info (pure Sdl) (progDesc "sdl simple direct media layer"))
      ]

data OptParseOptions = OptParseOptions
  { _hello      :: String
  , _quiet      :: Bool
  , _enthusiasm :: Int }

cliParser :: Parser OptParseOptions
cliParser = OptParseOptions
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "direction"
         <> short 'd'
         <> help "Go forward or backward true time" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )


main :: IO ()
main = program =<< customExecParser parserPrefs opts
  where
    opts = info (programParser <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )
    parserPrefs = prefs showHelpOnError

program :: Program -> IO ()
program = \case
  CLI opts -> greet opts
  Interact -> UI.Interact.main
  Yesod -> UI.Yesod.main
  Gtk -> UI.Gtk.main
  Sdl -> UI.Sdl.main

greet :: OptParseOptions -> IO ()
greet (OptParseOptions h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet (OptParseOptions h True n) = putStrLn $ "Bai, " ++ h ++ replicate n '!'
