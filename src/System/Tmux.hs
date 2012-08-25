{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module System.Tmux
  ( Split(..)
  , Pane(..)
  , Window(..)
  , Tmux(..)
  , TmuxF(..)
  , newWindow
  , renameWindow
  , selectWindow
  , splitWindow
  ) where

import Control.Monad.Free

data Split = Vert | Horiz
data Pane = Pane Window Integer
data Window = Window Integer

data TmuxF f = NewWindow String (Window -> f)
             | RenameWindow Window String (Window -> f)
             | SelectWindow Window (Window -> f)
             | SplitWindow Pane Split (Maybe Int) String (Pane -> f)
             deriving Functor

type Tmux = Free TmuxF

newWindow cmd = liftF $ NewWindow cmd id
renameWindow w str = liftF $ RenameWindow w str id
selectWindow w = liftF $ SelectWindow w id
splitWindow p sp sz cmd = liftF $ SplitWindow p sp sz cmd id
