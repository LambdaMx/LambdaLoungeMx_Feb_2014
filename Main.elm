import Mouse
import Keyboard
import Window
import Graphics.Element

greeting : Element
greeting = [markdown|
# ¡ Bienvenido a Lambda Lounge Mx !
|]

gratitude = [markdown|
¡Gracias por visitarnos! :)
|]

presentation = [markdown|
Lambda Lounge Mx es un evento para tí, que te interesa compartir experiencias y conocimiento relacionado a la programación funcional.
|]

header = flow down [ lambdaImg, greeting, gratitude ]

------------------------

lambdaImg = image 200 200 "img/AlonzoChurch.png"

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
  ]

logoWheel time =
  let alonzo = lambdaImg |> toForm |> move (0,0) 
      toRotatingLogo n angle = 
        drop n logos
        |> head
        |> toForm
        |> scale (1 + 0.2 * (sin <| (time * pi / 4000) + toFloat(n) * pi / 10 ))
        |> move (fromPolar (350, angle + pi - (time * pi / 6000)))
              
  in collage 900 900 <| alonzo :: (map (\n -> toRotatingLogo n (turns (toFloat(n)/9))) [0..8]) 

animation = lift logoWheel (foldp (+) 0 (fps 30))

--------------------

showMouse = lift asText Mouse.position

--------------------
type Input = { space:Bool, dirL:Int, dirR:Int, delta:Time }
delta = inSeconds <~ fps 30
input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .y Keyboard.wasd
                               ~ lift .y Keyboard.arrows
                               ~ delta)
--------------------


--main = lift asText input
main = animation 
--main = lift asText <| fps 30
--main = collage 500 500 <| map toForm logos
