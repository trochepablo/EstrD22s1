module Color where

data Color = RGB Int Int Int
     deriving (Eq, Show)
     {- INV.REP.: los 3 números están entre 0 y 254 -}

renderColor :: Color -> String
renderColor (RGB r g b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

black, white                   :: Color
red, green, blue               :: Color
yellow, orange, purple         :: Color
pink, cyan, brown              :: Color
green1, green2, green3, green4 :: Color
brown1, brown2                 :: Color
black  = RGB   0   0   0
white  = RGB 255 255 255
red    = RGB 255   0   0
green  = RGB   0 255   0
blue   = RGB   0   0 255
yellow = RGB 255 255   0
orange = RGB 255 180   0
purple = RGB 180   0 255
pink   = RGB 255   0 255
cyan   = RGB   0 255 255
brown  = RGB 120  60  60 
green1 = RGB   0 129   0
green2 = RGB   0 166   0
green3 = RGB   0 194   0
green4 = RGB 170 212   0
brown1 = RGB 170  83   0
brown2 = RGB 170 136   0