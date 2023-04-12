{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module UI.Gtk (main) where


import Control.Monad (void)
import qualified GI.Gtk as Gtk
import Data.GI.Base
import Data.Text(unpack, pack)

main :: IO ()
main = do
  Gtk.init Nothing

  window <- Gtk.new Gtk.Window [#title := "Haskell GTK+ Counter Example"]
  #resize window 300 200
  on window #destroy Gtk.mainQuit

  vbox <- Gtk.new Gtk.Box [#orientation := Gtk.OrientationVertical]
  #add window vbox

  label <- Gtk.new Gtk.Label [#label := "Counter: 0"]
  #packStart vbox label False False 0

  button <- Gtk.new Gtk.Button [#label := "Click me!"]
  #packStart vbox button False False 0

  let updateLabel count = do
        let newText = pack $ "Counter: " ++ show count
        #setText label newText

  button `on` #clicked $ do
    count <- #getText label >>= \t -> return (read (drop 9 (unpack t)) :: Int)
    updateLabel (count + 1)

  #showAll window
  Gtk.main
