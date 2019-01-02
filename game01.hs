import Control.Concurrent
import System.IO
import Data.Char                 


goto posX = putStr ("\ESC[" ++ show posX ++ ";0H")

main :: IO ()
main = do hSetBuffering stdin NoBuffering
          hSetBuffering stdout NoBuffering
          hSetEcho stdin False
          clear
          goto 10
          putStr "#"
          left
          save_cur
          goto 11
          many 1
          restore_cur         
          loop
          

loop = do
    --goto 0 
    i <- hGetChar stdin
    --goto 25
    input i
    loop

moveR = do
    putStr " "
    putStr "#"
    left
    
moveL = do
    putStr " "
    left
    left
    putStr "#"
    left

many x = do
    if x < 30 then do
        putStr "T"
        many (x + 1)
        else putStr ""

 

input x = do
    if x == 'w' then putStr "W" 
    else if x == 's' then moveL
    else if x == 'd' then moveR
    else if x == ' ' then do game_log "pressed space" ; jump 1
    else if x == 'a' then moveL else message_log

    --Terminal escapes sequence for cursor control
right = putStr "\ESC[1C"
left = putStr "\ESC[1D"
up = putStr "\ESC[1#A"
down = putStr "\ESC[1#B"
clear = putStr "\ESC[2J \ESC[0;0H"  -- \ESC[2J clear terminal scape sequence and \ESC[0;0H  move cursor to 0 , 0
clear_line = putStr "\ESC[K"

save_cur = putStr "\ESC[s"
restore_cur = putStr "\ESC[u"

message_log = do
    save_cur
    goto 24
    putStr "no input control"
    restore_cur

game_log text = do
    save_cur
    goto 24
    clear_line
    putStr text
    restore_cur


jump h = do
    if h < 5
    then do
    putStr " "
    left
    up
    putStr "#"
    left
    sleep 1
    jump $ h + 1
    else gravity 5
    
gravity i = do
    if i > 1
    then do
    putStr " "
    left
    down
    putStr "#"
    left
    sleep 1
    gravity $ i - 1
    else putStr ""

sleep t = threadDelay $ t * 100 * 1000





