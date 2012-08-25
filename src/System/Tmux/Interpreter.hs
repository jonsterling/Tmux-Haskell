module System.Tmux.Interpreter
  ( run ) where

import System.Tmux
import System.Process
import System.Exit
import Control.Monad.Free
import Control.Applicative
import Data.Char

run :: Tmux a -> IO a
run (Pure r) = return r
run (Free (NewWindow cmd f)) =
  rawNewWindow cmd >>= run . f
run (Free (RenameWindow w n f)) =
  rawRenameWindow w n >>= run . f
run (Free (SelectWindow w f)) =
  rawSelectWindow w >>= run . f
run (Free (SplitWindow p sp sz cmd f)) =
  rawSplitWindow p sp sz cmd >>= run . f

rawNewWindow cmd = do
  rawSystem "tmux" ["new-window", cmd]
  index <- readProcess "tmux" ["display-message", "-p", "'#I'"] ""
  return $ Window (parseIndex index)

rawRenameWindow w@(Window i) n =
  w <$ rawSystem "tmux" ["rename-window", "-t", show i, n]

rawSelectWindow w@(Window i) =
  w <$ rawSystem "tmux" ["select-window", "-t", show i]

rawSplitWindow (Pane w i) sp sz cmd =
  do run $ selectWindow w
     let target = ["-t", show i]
     let size = maybe [] (\percent -> ["-p", show percent]) sz
     let command = ["'" ++ cmd ++ "'"]
     rawSystem "tmux" ("split-window" : splitToOption sp : target ++ size ++ command)
     index <- readProcess "tmux" ["display-message", "-p", "'#P'"] ""
     return $ Pane w (parseIndex index)


splitToOption :: Split -> String
splitToOption Vert = "-v"
splitToOption Horiz = "-h"

parseIndex = read . filter isDigit

