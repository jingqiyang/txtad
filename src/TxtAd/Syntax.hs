{-# LANGUAGE TemplateHaskell, DeriveLift #-}
module TxtAd.Syntax where

-- hiding (Name) prevents type clash with alias for scene name
import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Lift

data TxtAd =
    Start Name -- declare start scene
  | Msg String -- declare error message to print for invalid player input
  | Def Name Scene -- define a scene
  deriving (Lift) 

type Input = String -- user input
type Name = String -- scene name

-- bool is true if scene is an end scene
-- string is all text to display when scene is loaded ("" if no scene text)
-- list is scene transitions: (user input, name of scene to transition to)
type Scene = (Bool, String, [(Input, Name)])