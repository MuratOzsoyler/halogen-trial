module Types where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

type StaticHTML = HH.ComponentHTML Unit () Aff

type StaticComponent = H.Component HH.HTML (Const Unit) Unit Void Aff
