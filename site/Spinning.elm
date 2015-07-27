import Mouse exposing(..)
import Keyboard exposing(..)
import Window exposing(..)
import List exposing(..)
import Signal exposing((<~), foldp)
import Time exposing(fps)
import Color exposing(..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)

alonzoImg = image 200 200 "img/AlonzoChurch.png" |> toForm

logos = map (image 120 120) [
  "img/clojure.png", 
  "img/elixir.png", 
  "img/elm.png", 
  "img/erlang.png", 
  "img/fsharp.png", 
  "img/haskell.png", 
  "img/mathematica.png",
  "img/scheme.png",
  "img/scala.png"
  ] |> map toForm

headForm xs =
  case (head xs) of
    Just x -> x
    _      -> circle 0.1 |> filled white

logoWheel time =
  let alonzo = alonzoImg |> move (0,0) 
      toRotatingLogo n angle = 
        drop n logos
        |> headForm
        |> scale (1 + 0.2 * (sin <| (time * pi / 4000) + toFloat(n) * pi / 10 ))
        |> move (fromPolar (350, angle + pi - (time * pi / 6000)))    
  in collage 900 900 <| alonzo :: map (\n -> toRotatingLogo n (turns (toFloat(n)/9))) [0..8] 

animation = logoWheel <~ (foldp (+) 0 (fps 30))

main = animation 
