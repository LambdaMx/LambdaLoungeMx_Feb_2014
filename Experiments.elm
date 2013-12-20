import Mouse
import Keyboard
import Window
import Graphics.Element

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
main = lift asText input
--main = lift asText <| fps 30
--main = collage 500 500 <| map toForm logos