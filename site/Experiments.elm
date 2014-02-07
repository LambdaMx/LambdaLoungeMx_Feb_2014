import Mouse
import Keyboard
import Window
import Graphics.Element
import Graphics.Collage

--------------------

showMouse = lift asText Mouse.position

--------------------
type Input = { space:Bool, dirL:Int, dirR:Int, pos: (Int,Int), delta:Time }
delta = inSeconds <~ fps 30
input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .y Keyboard.wasd
                               ~ lift .y Keyboard.arrows
                               ~ Mouse.position
                               ~ delta)
--------------------

logo = image 200 200 "img/AlonzoChurch.png" 
       |> toForm
       |> alpha 0.25

--------------------

roundedRect = rect 200 400

-- main = collage 500 500 [ roundedRect |> filled lightBlue ]

-------


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

--main =  invitation

main = lift asText input
--main = lift asText <| fps 30
--main = collage 500 500 [logo]