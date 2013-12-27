import Mouse
import Keyboard
import Window
import Graphics.Element

-- Content

invitation = [markdown|
# ¡ Bienvenido a Lambda Lounge Mx !
**Lambda Lounge Mx** es un evento para tí, que te interesa compartir experiencias 
y conocimiento relacionados al estilo de programación funcional, así como aprender 
más sobre éste tema a partir de las pláticas y exposiciones de otros participantes.

Es todo un día lleno de pláticas invitadas, discusiones y presentaciones de soluciones
a problemas específicos, todo desde el punto de vista de la **programación funcional**.

Lisp, Haskell, Erlang, ML, F#, Sala ... ¡todos son bienvenidos aquí! ... Bueno, casi todos :-P

### Agenda

La agenda del evento es la siguiente: 

|Inicio  | | |Fin    | | |Descripción
|:------:|-|-|:-----:|-|-|:----------
|10:00   | | |10:30  | | | Apertura
|10:30   | | |11:30  | | | "Introducción al cálculo lambda" por @sergiodnila
|11:30   | | |13:00  | | | Presentaciones de soluciones al problema 1
|13:00   | | |14:00  | | | "Introducción a matemática" por @lux_spes
|14:00   | | |15:00  | | | Comida
|15:00   | | |16:30  | | | Presentaciones de soluciones al problema 2
|16:30   | | |17:30  | | | "Aplicaciones masivamente concurrentes" por @hiphoox
|17:30   | | |18:00  | | | Cierre

### Lugar y Fecha

La cita es el próximo 25 de Enero del 2014 en Uny-II.

Calle San Lorenzo #1009, piso 4. Colonia del Valle. 
Delegación Benito Juárez, México, D.F.

### Registro

Solo hay dos requisitos para que participes:

1. Que resuelvas en un lenguaje o estilo funcional uno de los [dos problemas]() 
que hemos planteado y que estés dispuesto a platicarnos tu solución (código) en 
un espacio de 25 minutos.

2. Que te registres [aquí]()

### Trivia
Al entrar a esta página aparece el rostro estilizado de un personaje
histórico para las ciencias de la computación. ¿Sabes quién es y puedes adivinar
por qué lo incluimos? Después de registrarte en el evento, envía tu respuesta
[aquí](http://j.mp/19l2COA). Las primeras 3 personas en responder acertadamente 
recibirán un premio sorpresa el día del evento ;-)
|]

-- App parameters

defaultRadius    = 350
defaultTxtWidth  = 500
defaultTxtHeight = 1200
defaultLogoSize  = 200

-- Aux functions

asPoint (x,y) = (toFloat x, toFloat y)

distance2Center (x,y) (w,h) = 
  let hw = w / 2
      hh = h / 2
  in sqrt ((x - hw)^2 + (y - hh)^2) / sqrt (hw^2 + hh^2)

-- #TODO Abstract rotation by means of a velocity parameter (rad/seg)
indexedRotationAngle i n time = (turns <| toFloat(i) / toFloat(n) ) - (time * pi / 6000)

-- #TODO Abstract 
indexedSin i time = sin <| (time * pi / 4000) + toFloat(i) * 3  * pi / 5 

-- Images

lambdaImg = image defaultLogoSize defaultLogoSize "img/LambdaMx.png"     |> toForm
alonzoImg = image defaultLogoSize defaultLogoSize "img/AlonzoChurch.png" |> toForm

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

-- Page Model

data State = Approaching | Spinning | Redirecting | Inviting

type Point = (Float, Float)
type Logo  = { img: Form, pos: Point, scale: Float, vx: Float, vy: Float }
type Page  = { center: Logo, logos: [Logo], content: Form, state:State }
type Input = { click: Bool, ttime: Time, delta: Time, pos: Point, dim: (Int,Int) }

initialPage : Page
initialPage = 
  let idxLogos       = zip [0..(length logos)-1] logos
      moveLogo (i,l) = { img = l
                       , pos = positionFor i (length logos) 0
                       , scale = 1
                       , vx = 0
                       , vy = 0 
                       }
  in { center  = { img = alonzoImg, pos = (0,0), scale = 1, vx = 0, vy = 0 }
     , logos   = idxLogos |> map moveLogo
     , content = invitation |> width defaultTxtWidth 
                            |> container defaultTxtWidth defaultTxtHeight midTop 
                            |> toForm 
                            |> alpha 0
     , state   = Approaching
     }

stepPage : Input -> Page -> Page
stepPage {click, ttime, delta, pos, dim} ({center, logos, content, state} as page) = 
  let newState = if | state == Approaching && click -> Spinning
                    | state == Spinning && click    -> Redirecting
                    | state == Redirecting          -> Inviting
                    | otherwise                     -> state
  in { page | content <- stepContent newState content
            , center  <- stepCenter  newState center  pos dim
            , logos   <- stepLogos   newState logos   pos dim ttime delta
            , state   <- newState
     }

stepContent : State -> Form -> Form
stepContent state content = 
  case state of
  Inviting -> content |> alpha 1
  _        -> content

stepCenter : State -> Logo -> Point -> (Int,Int) -> Logo
stepCenter state logo pos dim =
  let newScale = 1 + 0.25 * (distance2Center pos (asPoint dim))
  in case state of
    Approaching -> { logo | scale <- newScale }
    Spinning    -> { logo | scale <- newScale, img <- lambdaImg }
    Inviting    -> { logo | img <- scale 0 logo.img }
    Redirecting -> { logo | img <- scale 0 logo.img }

stepLogos : State -> [Logo] -> Point -> (Int, Int) -> Time -> Time -> [Logo]
stepLogos state logos pos dim time delta  = 
  let newAlpha            = 1 - distance2Center pos (asPoint dim)
      toAlpha logo        = { logo | img <- logo.img |> alpha newAlpha }
      toAlpha25 logo      = { logo | img <- logo.img |> alpha 0.25 }
      idxLogos            = zip [0..(length logos)-1] logos
      spin (i,logo)       = (i, { logo | pos <-  positionFor i (length logos) time })
      resizeLogo (i,logo) = (i, { logo | scale <- 1 + 0.2 * (indexedSin i time) })
      unindex (i,logo)    = logo
      angle (r,theta)     = theta
      toLinearMov logo    = { logo | vx <- 150 * cos ((angle . toPolar) logo.pos) 
                                   , vy <- 150 * sin ((angle . toPolar) logo.pos) }
      xpos logo           = fst logo.pos
      ypos logo           = snd logo.pos
      halfWindowWidth     = toFloat (fst dim) / 2
      halfWindowHeight    = toFloat (snd dim) / 2
      bounceMargin logo   = defaultLogoSize * logo.scale / 2
      moveLinear logo     = { logo | pos <- ( xpos logo + logo.vx * delta , 
                                              ypos logo + logo.vy * delta )
                                   , vx <- stepV logo.vx (xpos logo > (halfWindowWidth-(bounceMargin logo))) 
                                                         (xpos logo < -(halfWindowWidth-(bounceMargin logo)))
                                   , vy <- stepV logo.vy (ypos logo > (halfWindowHeight-(bounceMargin logo))) 
                                                         (ypos logo < -(halfWindowHeight-(bounceMargin logo)))
                            }
  in case state of
    Approaching  -> logos    |> map toAlpha
    Spinning     -> idxLogos |> map (unindex . spin . resizeLogo) 
    Inviting     -> idxLogos |> map (unindex . resizeLogo) |> map (moveLinear . toAlpha25)
    Redirecting  -> logos    |> map toLinearMov

stepV v upperCollision lowerCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

positionFor : Int -> Int -> Time -> Point
positionFor i n time = fromPolar (defaultRadius, indexedRotationAngle i n time)

-- Main. Links input signal with page model evolution and makes display

display : (Int,Int) -> Page -> Element
display (w,h) {center, logos, content, state} = 
  let draw logo = logo.img |> move logo.pos |> scale logo.scale
  in collage w (max h defaultTxtHeight) <| 
     draw center :: (map draw logos) ++ [content]

input = 
  let source = fps 30
      ttime  = foldp (+) 0 source
  in sampleOn ttime <| Input <~ Mouse.isDown
                              ~ ttime
                              ~ (inSeconds <~ source)
                              ~ (asPoint <~ Mouse.position)
                              ~ Window.dimensions
main = 
  let pageState = foldp stepPage initialPage input
  in display <~ Window.dimensions ~ pageState
