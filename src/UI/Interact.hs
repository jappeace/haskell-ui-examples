module UI.Interact
  ( main
  ) where
import System.IO

-- https://hackage.haskell.org/package/base-4.18.0.0/docs/Prelude.html#v:interact
-- interact2   ::  (String -> String) -> IO ()
-- interact2 f =   do s <- getLine
--                    putStrLn (f s)

interaction :: String -> String
interaction "jappie" = "hi\n"
interaction "jakob" = "hello\n"
interaction x = "unkown input: '" <> x <> "'\n"

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout NoBuffering
  interact (concatMap interaction . lines)
