module Interactive where

import Debug
import List
import List((::))
import Signal
import Html
import Html(..)
import Html.Attributes
import Html.Attributes(class, style, width, height, rel, type',href)
import Html.Events (onClick, on)
import Html.Events
import Poly(..)
import Window

type Model
  = LeftOpen  (Model, () -> Model) (() -> Model)
  | RightOpen (() -> Model) (Model, () -> Model)
  | Join Model Model
  | Constant String

switch : Position -> Model -> Model
switch p m = case (p, m) of
  ([], LeftOpen (l, fl) fr)  -> RightOpen fl (fr (), fr)
  ([], RightOpen fl (r, fr)) -> LeftOpen (fl (), fl) fr

  (Continue :: ds, LeftOpen (l, fl) fr)  -> LeftOpen (switch ds l, fl) fr
  (Continue :: ds, RightOpen fl (r, fr)) -> RightOpen fl (switch ds r, fr)
  (Left :: ds, Join l r)                 -> Join (switch ds l) r
  (Right :: ds, Join l r)                -> Join l (switch ds r)
  _                                      -> m

type Update
  = NoOp
  | Switch Position
  | SetModel Model

update : Update -> Model -> Model
update u m = case u of
  NoOp        -> m
  Switch p    -> switch p m
  SetModel m' -> m'

updateChan : Signal.Channel Update
updateChan = Signal.channel NoOp

render : Model -> Html
render =
  let go pos m =
        case m of
          LeftOpen (l, _) _ ->
            div [class "add"]
            [ div [class "left"] [go (Continue :: pos) l]
            , div
              [ onClick (Signal.send updateChan (Switch (List.reverse pos)))
              , class "cover right"
              ]
              []
            ]

          RightOpen _ (r, _) ->
            div [class "add"]
            [ div
              [ onClick (Signal.send updateChan (Switch (List.reverse pos)))
              , class "cover left"
              ]
              []
            , div [class "right"] [go (Continue :: pos) r]
            ]

          Join ml mr ->
            div [class "mul"]
            [ div [] [go (Left :: pos) ml]
            , div [] [go (Right :: pos) mr]
            ]

          Constant s ->
            div [class "constant"]
            [ span [style [("position", "relative"), ("top", "50%")]] [text s]
            ]
  in
  go []

modelable : Poly -> Bool
modelable p = case p of
  Var        -> False
  Add p q    -> modelable p || modelable q
  Mul p q    -> modelable p && modelable q
  Const _ -> True

modelPoly : Poly -> Maybe Model
modelPoly p = 
  let go q = case q of
        Const s -> Constant s
        Mul l r -> Join (go l) (go r)
        -- Not sure if right
        Add l r ->
          if modelable l
          then LeftOpen (go l, \() -> go l) (\() -> go r)
          else RightOpen (\() -> go l) (go r, \() -> go r)

        -- Since the poly is modelable, this call is safe
        Var -> go p
  in
  -- Some redundant computations...
  if modelable p then Just (go p) else Nothing

type Dir = Left | Continue | Right
type alias Position = List Dir

-- parsePoly = 

scene m (w, h) =
  let d = toString (min w h) ++ "px" 
      css = style [("width", d), ("height", d)]
  in
  div []
  [ node "link" [rel "stylesheet", type' "text/css", href "www/tree.css"] []
  , div [css] [render m]
  ]
  |> Html.toElement w h

state m0 =
  Signal.foldp update m0 (Signal.map (Debug.watch "Update") (Signal.subscribe updateChan))
  |> Signal.map (Debug.watch "Model")

nat = Add (Const "()") Var
binTree = Add (Const "()") (Mul Var Var)
twoThree = Add (Const "") (Add (Mul Var Var) (Mul Var (Mul Var Var)))
ab = Add (Const "A") (Const "B")

main =
  let poly  = ab
      m0May = modelPoly poly
  in
  case m0May of
    Just m0 -> Signal.map2 scene (state m0) Window.dimensions
    Nothing -> Signal.map (\(w,h) -> Html.toElement w h (text "No fixed point")) Window.dimensions

