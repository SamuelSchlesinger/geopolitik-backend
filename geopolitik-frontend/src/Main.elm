module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http

main = Browser.sandbox { init = init, update = update, view = view }

type alias User = 
  { username : String }

type alias Article =
  { name : String
  , lines : Array String }

type Model 
  = SignedOff
  | SignedOn
  | Editing Article

type Message
  = SignIn String String

init = SignedOff

update message model = case model of
  SignedOff -> case message of
    SignIn username password -> post 
    { 
      url = "/account/signin",
      body = jsonBody 
      {
        signInUsername = username
      , signInPassword = password
      }
    }
