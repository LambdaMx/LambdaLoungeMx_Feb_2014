import Mouse
import Keyboard
import Window
import Graphics.Element

-- Content

invitation = [markdown|
# Bienvenido a Lambda Lounge Mx !
Lambda Lounge Mx es un evento para tí, que te interesa compartir experiencias 
y conocimiento relacionado a la programación funcional, así como aprender de 
otras personas sobre estos temas. El único requisito para que participes es
que resuelvas en un lenguaje o estilo funcional cualquiera de los [dos 
problemas](./problemas.elm) que hemos planteado y que estés dispuesto a exponer tu 
solución en un espacio de 25 minutos. La agenda del evento es la siguiente: 

|Inicio|Fin|Descripción
|------|---|-----------
|10:00|10:30| Opening
|10:30|11:30| Keynote por @sergiodnila
|11:30|13:00| Presentaciones de soluciones a los problemas
|13:00|14:00| Keynote por @lux_spes
|14:00|15:00| Comida
|15:00|16:30| Presentaciones de soluciones a los problemas.
|16:30|17:30| Keynote por @hiphoox
|17:30|18:00| Cierre
|]

-- Data definitiona

data State = Approaching | Spinning | Inviting

type Point = (Float, Float)
type Logo  = { img: Form, pos: Point }
type Page  = { center: Logo, logos: [Logo], content: Form, state:State }
type Input = { click: Bool, delta: Time, pos: Point }

-- App parameters

defaultRadius = 350

-- Aux functions

asPoint (x,y) = (toFloat x, toFloat y)

-- Images

lambdaImg = image 200 200 "img/LambdaMx.png"     |> toForm
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


-- Page Logic

initialPage : Page
initialPage = 
  let idxLogos       = zip [0..(length logos)-1] logos
      position i     = positionFor i (length logos) 
      moveLogo (i,l) = { img = l, pos = position i 0 }
  in
    { center  = { img = alonzoImg |> alpha 0, pos = (0,0) }
    , logos   = idxLogos |> map moveLogo
    , content = invitation |> toForm |> alpha 0
    , state   = Approaching
    }

stepPage : Input -> Page -> Page
stepPage {click, delta, pos} ({center, logos, content, state} as page) = 
  let idxLogos            = zip [0..(length logos)-1] <| map .img logos
      position i          = positionFor i (length logos) 
      moveLogo (i,l)      = { img = l, pos = position i delta }
      resizeLogo (i,logo) = (i, resize i delta logo)
      newState            = if | state == Approaching && click -> Spinning
                               | state == Spinning && click    -> Inviting
                               | otherwise                     -> state
      toAlpha logo       = {logo| img <- relativeAlpha pos logo.img }
  in {page | center  <- case state of
                          Approaching -> { img = alonzoImg, pos = (0,0) }
                          Spinning    -> { img = lambdaImg, pos = (0,0) }
                          Inviting    -> { img = lambdaImg |> scale 0, pos = (0,0) }
           , logos   <- case state of
                          Approaching -> logos    |> map toAlpha
                          _           -> idxLogos |> map (moveLogo . resizeLogo) 
           , content <- case state of
                          Inviting -> content |> alpha 1
                          _        -> content 
           , state   <- newState
     }

-- #TODO Abstract rotation by means of a velocity parameter (rad/seg)
positionFor : Int -> Int -> Time -> Point
positionFor i n time = 
  let angle  = (turns <| toFloat(i) / toFloat(n) ) + pi - (time * pi / 6000)
  in fromPolar (defaultRadius, angle)

-- #FIXME it depends on time, not on the previous state
-- #TODO abstract velocity of expansion/contraction by means of a parameter (rad/seg ?)
resize : Int -> Time -> Form -> Form
resize i time form = 
  scale (1 + 0.2 *  (sin <| (time * pi / 4000) + toFloat(i) * pi / 10 )) <| head (drop i logos)

distance2Center (x,y) = sqrt (x^2 + y^2) / defaultRadius

relativeAlpha pos f  = alpha (distance2Center pos) f

-----------------
delta = foldp (+) 0 (fps 30) -- inSeconds <~fps 30
input = sampleOn delta <| Input <~ Mouse.isDown
                                 ~ delta
                                 ~ (asPoint <~ Mouse.position)

pageState = foldp stepPage initialPage input

-----------------

display : (Int,Int) -> Page -> Element
display (w,h) {center, logos, content, state} = 
  let draw {img,pos} = move pos img
  in collage w h <| draw center :: (map draw logos) ++ [content]

main = lift2 display Window.dimensions pageState

