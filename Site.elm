import Mouse
import Keyboard
import Window
import Graphics.Element

-- Content

invitation = [markdown|
# ¡ Bienvenido a Lambda Lounge Mx !
**Lambda Lounge Mx** es un evento para tí, que te interesa compartir experiencias 
y conocimiento relacionado a la programación funcional, así como aprender de 
otras personas sobre estos temas. El único requisito para que participes es
que resuelvas en un lenguaje o estilo funcional cualquiera de los [dos 
problemas](./problemas.elm) que hemos planteado y que estés dispuesto a exponer
tu solución en un espacio de 25 minutos. La agenda del evento es la siguiente: 

|Inicio|Fin|Descripción
|------|---|-----------
|10:00|10:30| Opening
|10:30|11:30| Plática por @sergiodnila
|11:30|13:00| Presentaciones de soluciones a los problemas
|13:00|14:00| Plática por @lux_spes
|14:00|15:00| Comida
|15:00|16:30| Presentaciones de soluciones a los problemas.
|16:30|17:30| Plática por @hiphoox
|17:30|18:00| Cierre
|]

-- Data definitiona

data State = Approaching | Spinning | Inviting

type Point = (Float, Float)
type Logo  = { img: Form, pos: Point, scale: Float }
type Page  = { center: Logo, logos: [Logo], content: Form, state:State }
type Input = { click: Bool, delta: Time, pos: Point, dim: (Int,Int) }

-- App parameters

defaultRadius = 350

-- Aux functions

asPoint (x,y) = (toFloat x, toFloat y)

-- Images

lambdaImg = image 200 200 "img/LambdaMx.png"     |> toForm
alonzoImg = image 200 200 "img/AlonzoChurch.png" |> toForm

logos = map (image 120 120) [
  "img/clojure.png",
  "img/mathematica.png",
  "img/haskell.png",
  "img/erlang.png",
  "img/fsharp.png",
  "img/scheme.png",
  "img/elixir.png",
  "img/elm.png",
  "img/scala.png"
  ] |> map toForm


-- Page Logic

initialPage : Page
initialPage = 
  let idxLogos       = zip [0..(length logos)-1] logos
      position i     = positionFor i (length logos) 
      moveLogo (i,l) = { img = l, pos = position i 0, scale = 1 }
  in { center  = { img = alonzoImg, pos = (0,0), scale = 1 }
     , logos   = idxLogos |> map moveLogo
     , content = invitation |> toForm |> alpha 0
     , state   = Approaching
     }

stepPage : Input -> Page -> Page
stepPage {click, delta, pos, dim} ({center, logos, content, state} as page) = 
  let newState = if | state == Approaching && click -> Spinning
                    | state == Spinning && click    -> Inviting
                    | otherwise                     -> state
  in { page | center  <- stepCenter  newState center
            , content <- stepContent newState content
            , logos   <- stepLogos   newState delta pos dim logos
            , state   <- newState
     }

stepCenter : State -> Logo -> Logo
stepCenter state logo = 
  case state of
  Approaching -> logo
  Spinning    -> { logo | img <- lambdaImg }
  Inviting    -> { logo | img <- scale 0 lambdaImg }

stepLogos : State -> Time -> Point -> (Int, Int) -> [Logo] -> [Logo]
stepLogos state delta pos dim logos = 
  let toAlpha logo        = {logo| img <- relativeAlpha pos dim logo.img }
      idxLogos            = zip [0..(length logos)-1] logos
      moveLogo (i,logo)   = (i, { logo | pos <-  position i delta })
      resizeLogo (i,logo) = (i, { logo | scale <- 1 + 0.2 * (indexedSin i delta) })
      unindex (i,logo)    = logo
      position i          = positionFor i (length logos) 
  in case state of
    Approaching -> logos    |> map toAlpha
    _           -> idxLogos |> map (unindex . moveLogo . resizeLogo) 

stepContent : State -> Form -> Form
stepContent state content = 
  case state of
  Inviting -> content |> alpha 1
  _        -> content   

-- #TODO Abstract rotation by means of a velocity parameter (rad/seg)
positionFor : Int -> Int -> Time -> Point
positionFor i n time = 
  let angle  = (turns <| toFloat(i) / toFloat(n) ) - (time * pi / 6000)
  in fromPolar (defaultRadius, angle)

-- #FIXME it depends on time, not on the previous state
-- #TODO abstract velocity of expansion/contraction by means of a parameter (rad/seg ?)

indexedSin i time = sin <| (time * pi / 4000) + toFloat(i) * 3  * pi / 5 

resize : Int -> Time -> Form -> Form
resize i time form = 
  scale (1 + 0.2 * (indexedSin i time)) <| head (drop i logos)

distance2Center (x,y) (w,h) = 
  let hw = w / 2
      hh = h / 2
  in sqrt ((x - hw)^2 + (y - hh)^2) / sqrt (hw^2 + hh^2)

relativeAlpha pos dim f = alpha (1 - distance2Center pos (asPoint dim)) f

-----------------
delta = foldp (+) 0 (fps 30) -- inSeconds <~fps 30
input = sampleOn delta <| Input <~ Mouse.isDown
                                 ~ delta
                                 ~ (asPoint <~ Mouse.position)
                                 ~ Window.dimensions

pageState = foldp stepPage initialPage input

-----------------

display : (Int,Int) -> Page -> Element
display (w,h) {center, logos, content, state} = 
  let draw logo = logo.img |> move logo.pos |> scale logo.scale
  in collage w h <| draw center :: (map draw logos) ++ [content]

main = lift2 display Window.dimensions pageState

