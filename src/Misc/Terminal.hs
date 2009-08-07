module Misc.Terminal (

    esc

  , clearAll
  , clearEol
  , clear   

  , move
  , moveUp     
  , moveDown   
  , moveBack   
  , moveForward

  , save
  , load

  , clr
  , fg
  , bg

  , normal   
  , bold     
  , faint    
  , standout 
  , underline
  , blink    
  , inverse  
  , invisible

  , Color (..)

  , reset
  , black  
  , red    
  , green  
  , yellow 
  , blue   
  , magenta
  , cyan   
  , white  

  , blackBold  
  , redBold    
  , greenBold  
  , yellowBold 
  , blueBold   
  , magentaBold
  , cyanBold   
  , whiteBold  

  , blackBg  
  , redBg    
  , greenBg  
  , yellowBg 
  , blueBg   
  , magentaBg
  , cyanBg   
  , whiteBg  
  , resetBg  

  , width
  , height
  , geometry
  ) where

import Control.Applicative
import Data.List (intercalate)
import System.Environment (getEnvironment)

--------[ ansi escape sequence generation ]------------------------------------

-- Generic function for producing ANSI escape sequences.
esc :: String -> [String] -> String -> String
esc a args b = concat ["\ESC[", a, intercalate ";" $ args, b]

-- Clear screen and end-of-line
clearAll, clearEol, clear :: String
clearAll = esc "2J" [] ""
clearEol = esc "K"  [] "" 
clear    = clearAll ++ move 1 1

-- Move the cursor to the specified row and column.
move :: Int -> Int -> String
move row col = esc "" [show col, show row] "H"

-- Relative cursor movements.
moveUp, moveDown, moveBack, moveForward :: Int -> String

moveUp      rs = esc "" [show rs] "A"
moveDown    rs = esc "" [show rs] "B"
moveBack    cs = esc "" [show cs] "D"
moveForward cs = esc "" [show cs] "C"

-- Load and store the current cursor position.
save :: String
save = esc "s" [] ""

load :: String
load = esc "u" [] ""

-- Generic function for creating (foreground) color sequences.
clr :: [String] -> String
clr codes = esc "" codes "m"

-- Create foreground and background colors.
fg :: Color -> [String]
fg c = [show ((num c :: Int) + 30)]

bg :: Color -> [String]
bg c = [show ((num c :: Int) + 40)]

-- Style modifiers.
normal, bold, faint, standout, underline, blink, inverse, invisible
  :: [String] -> [String]

normal    = ("0":)
bold      = ("1":)
faint     = ("2":)
standout  = ("3":)
underline = ("4":)
blink     = ("5":)
inverse   = ("7":)
invisible = ("8":)

--------[ ansi color listing ]-------------------------------------------------

data Color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Reset
  deriving (Show, Eq)

-- Ansi codes offsets for color values.
num :: Num a => Color -> a
num Black   = 0
num Red     = 1
num Green   = 2
num Yellow  = 3
num Blue    = 4
num Magenta = 5
num Cyan    = 6
num White   = 7
num Reset   = 9

--------[ shortcut functions for common actions ]------------------------------

-- Reset all color and style information.
reset :: String
reset = esc "" ["0", "39", "49"] "m"

-- Shortcut for setting foreground colors.
black, red, green, yellow, blue,
  magenta, cyan, white :: String

black   = clr $ fg Black
red     = clr $ fg Red
green   = clr $ fg Green
yellow  = clr $ fg Yellow
blue    = clr $ fg Blue
magenta = clr $ fg Magenta
cyan    = clr $ fg Cyan
white   = clr $ fg White

-- Shortcut for setting bold foreground colors.
blackBold, redBold, greenBold, yellowBold, blueBold,
  magentaBold, cyanBold, whiteBold :: String

blackBold   = clr $ bold $ fg Black
redBold     = clr $ bold $ fg Red
greenBold   = clr $ bold $ fg Green
yellowBold  = clr $ bold $ fg Yellow
blueBold    = clr $ bold $ fg Blue
magentaBold = clr $ bold $ fg Magenta
cyanBold    = clr $ bold $ fg Cyan
whiteBold   = clr $ bold $ fg White

-- Shortcut for setting background colors.
blackBg, redBg, greenBg, yellowBg, blueBg,
  magentaBg, cyanBg, whiteBg, resetBg :: String

blackBg   = clr $ bg Black
redBg     = clr $ bg Red
greenBg   = clr $ bg Green
yellowBg  = clr $ bg Yellow
blueBg    = clr $ bg Blue
magentaBg = clr $ bg Magenta
cyanBg    = clr $ bg Cyan
whiteBg   = clr $ bg White
resetBg   = clr $ bg Reset

--------[ terminal geometry ]--------------------------------------------------

-- Try to read terminal width from environment variable.
width :: IO Int
width = (maybe 80 read . lookup "COLUMNS") <$> getEnvironment

-- Try to read terminal height from environment variable.
height :: IO Int
height = (maybe 24 read . lookup "LINES") <$> getEnvironment

-- Try to read terminal width and height from environment variables.
geometry :: IO (Int, Int)
geometry = (,) <$> width <*> height

