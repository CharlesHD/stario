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
import Text exposing (..)
import String
import Array

main =
  program { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
          }
-- UTIL
either : Maybe a -> b -> (a -> b) -> b
either m not f = case m of
                   Nothing -> not
                   Just v -> f v


-- MODEL
init = (Model 600 450 Dict.empty Dict.empty (CommandWindow "" []),  Cmd.none)

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

addShip : String -> Model -> Model
addShip name model =
  let ship = { x = 0,
               y = 0,
               speed = 0.005,
               rot = 0,
               rotSpeed = 0.001,
               wantedRot = 0,
               powerOn = True
             }
  in log ("created a ship named " ++ name) {model | ships = Dict.insert name ship model.ships}

moveShip : Float -> String -> Ship -> Ship
moveShip t name ship =
  let {x, y, speed, rot, rotSpeed, wantedRot, powerOn} = ship
      potRot = rotSpeed * t
      rotShip = if abs (wantedRot - rot) <= potRot
                then {ship | rot = wantedRot }
                else if wantedRot > rot
                     then {ship | rot = rot + potRot}
                     else {ship | rot = rot - potRot}
      dep = speed * t
  in if powerOn
     then {rotShip | x = x + dep * cos ship.rot, y = y + dep * sin ship.rot}
     else rotShip

updateShip : (Ship -> Ship) -> String -> Model -> Model
updateShip f name model =  if Dict.member name model.ships
                           then {model | ships = Dict.update name (Maybe.map f) model.ships}
                           else log ("there is no ship named" ++ name) model

turnShip : Float -> Ship -> Ship
turnShip deg ship = {ship | wantedRot = ship.wantedRot + deg}

stopShip : Ship -> Ship
stopShip ship = {ship | powerOn = False}

startShip : Ship -> Ship
startShip ship = {ship | powerOn = True}

log : String -> Model -> Model
log msg model = let cmds = model.cmds
                    ncmds = {cmds | messages = msg :: cmds.messages}
                in {model | cmds = ncmds}

clearInput : Model -> Model
clearInput model = let cmds = model.cmds
                       ncmds = {cmds | input = ""}
                   in {model | cmds = ncmds}

-- UPDATE
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
  let cmdargs = String.words model.cmds.input
      nmodel = clearInput model
      cmdMaybe = List.head cmdargs
      args = Maybe.map Array.fromList <| List.tail cmdargs
  in case cmdMaybe of
       Nothing -> nmodel
       Just cmd -> case cmd of
                     "create" -> either (Maybe.andThen args (Array.get 0))
                                 (log "create take a name as parameter" nmodel)
                                 (\x -> addShip (String.trim x) nmodel)
                     "turn" -> let name = Maybe.andThen args (Array.get 0)
                                   sdeg = Maybe.andThen args (Array.get 1)
                                   deg = Maybe.andThen sdeg (Result.toMaybe << String.toFloat)
                                   pair = Maybe.map2 (\x y -> (x,y)) name deg
                               in either pair
                                    (log ("Turn take a name and a degree") nmodel)
                                    (\ (n,r) -> updateShip (turnShip (degrees r)) n (log ("turning " ++ n ++ " by " ++ toString r ++ " degrees") nmodel))
                     "stop" -> either (Maybe.andThen args (Array.get 0))
                                      (log "stop take a ship name as a parameter" nmodel)
                                      (\x -> updateShip stopShip x (log ("stopped " ++ x) nmodel))
                     "start" -> either (Maybe.andThen args (Array.get 0))
                                      (log "start take a ship name as a parameter" nmodel)
                                      (\x -> updateShip startShip x (log ("started " ++ x) nmodel))


                     otherwise -> log ("Unknown command : " ++ cmd) nmodel

-- SUBSCRIPTIONS

subscriptions model = AnimationFrame.diffs Tick

-- VIEWS

view model =
  let ships = Dict.values <| Dict.map viewShip model.ships
      background = rect model.width model.height |> filled black
      elems = background :: ships
      (w', h') = (round model.width, round model.height)
  in div []
         [ collage w' h' elems |> toHtml
         , input [onInput Input, value model.cmds.input] []
         , button [onClick Send] [Html.text "Send"]
         , div [] <| List.map viewMessage (List.reverse <| List.take 5 model.cmds.messages)
         -- , div [] <| List.map (viewMessage << toString) (Dict.toList model.ships)
         ]

viewMessage msg =
  div [] [ Html.text msg ]


viewShip : String -> Ship -> Form
viewShip name ship =
  let shipgon = ngon 3 10
      nametext =move (-10, -10) <| Collage.text <| Text.height 20 <| Text.color white (fromString name)
      shipform = filled red shipgon
      translat = translation ship.x ship.y
      rot = rotation ship.rot
      transfo = multiply translat rot
  in groupTransform transfo [ shipform
                            , nametext
                            ]
