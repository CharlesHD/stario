import Html exposing (Html, Attribute, text, div, input, button)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Window exposing (Size)
import AnimationFrame
import Task
import Dict exposing (Dict)
import Collage exposing (..)
import Transform exposing (..)
import Element exposing (..)
import Color exposing (..)

main =
  program { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
          }

init = (Model 800 600 Dict.empty Dict.empty (CommandWindow "" []),  Cmd.none)

type alias Ship = {
    x : Float,
    y : Float,
    speed : Float,
    rot : Float,
    rotSpeed : Float,
    wantedRot : Float,
    powerOn : Bool
  }
type alias Squadron = List String
type alias CommandWindow = { input : String, messages : List String}

type alias Model =
                 {
                   width : Float,
                   height : Float,
                   ships : Dict String Ship,
                   squadrons : Dict String Squadron,
                   cmds : CommandWindow
                 }

type Msg = Input String
         | Send
         | Tick Float

update msg model =
  case msg of
    Input newInput ->
      let cmds = model.cmds in ({ model | cmds = {cmds | input = newInput}}, Cmd.none)
    Send ->
      (parseCommand model , Cmd.none)
    Tick t ->
      (gameTick t model, Cmd.none)

gameTick : Float -> Model -> Model
gameTick t model
         = {model | ships = Dict.map (moveShip t) model.ships}

parseCommand model =
  let name = model.cmds.input
  in addShip name model

addShip : String -> Model -> Model
addShip name model =
  let ship = { x = 0,
               y = 0,
               speed = 10,
               rot = 0,
               rotSpeed = 1,
               wantedRot = 0,
               powerOn = True
             }
  in {model | ships = Dict.insert name ship model.ships}

moveShip : Float -> String -> Ship -> Ship
moveShip t name ship =
  let {x, y, speed, rot, rotSpeed, wantedRot, powerOn} = ship
      potRot = rotSpeed * t
      rotShip = if abs (wantedRot - rot) <= potRot
                then ship
                else if wantedRot > rot
                     then {ship | rot = rot + potRot}
                     else {ship | rot = rot - potRot}
      dep = speed * t
  in if powerOn
     then {rotShip | x = x + cos ship.rot, y = y + sin ship.rot}
     else rotShip


subscriptions model = AnimationFrame.diffs Tick

view model =
  let ships = List.map viewShip (Dict.values model.ships)
      background = rect model.width model.height |> filled black
      elems = background :: ships
      (w', h') = (round model.width, round model.height)
  in div []
         [ collage w' h' elems |> toHtml
         , input [onInput Input, value model.cmds.input] []
         , button [onClick Send] [Html.text "Send"]
         , div [] (List.map viewMessage (List.reverse model.cmds.messages))
         ]

viewMessage msg =
  div [] [ Html.text msg ]

viewShip : Ship -> Form
viewShip ship =
  let shipgon = ngon 3 10
      shipform = filled green shipgon
      translat = translation ship.x ship.y
      rot = rotation ship.rot
      transfo = multiply translat rot
  in groupTransform transfo [shipform]
