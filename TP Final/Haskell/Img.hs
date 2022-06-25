module Img(
    module C
  , Width, Height, Size, Img, Dir(..)
  , createImg, sizeImg, compressImg, loadImg
  , writeImg, renderImg
) where

import qualified Matrix as M
import qualified Color  as C

type Width  = Int
type Height = Int
type Size   = Int

data Img = IT Width Height Size ITree
     deriving (Eq, Show)
     {- INV.REP.: IT w h s t
        * s = size t
        * s <= w*h
        * en t, 
           - cada nodo H tiene todos hijos V o S
           - cada nodo V tiene todos hijos H o S
     -}
data Dir = H | V     
     deriving (Eq, Ord, Show)
data ITree  = N Dir ITree ITree | S C.Color
     deriving (Eq, Show)

createImg   :: [[ C.Color ]] -> Img
loadImg     :: M.Matrix C.Color -> Img 
  -- PRECOND: w y h son iguales, y potencias de 2
sizeImg     :: Img -> Int
compressImg :: Img -> Img

sizeImg (IT _ _ s _) = s

writeImg :: Img -> IO ()
writeImg = writeFile "output.svg" . renderImg

compressImg (IT w h _ t) = IT w h ns nt
  where (ns, nt) = compressIT t

createImg []  = error "There are no empty images"
createImg xss = let ch = length xss
                    cw = maximum (map length xss)
                    h = nextPow2 ch
                    w = nextPow2 cw
                    n = max h w
                    nxs = pad n (replicate n C.black)
                                (map (pad n C.black) xss)
                    mx = M.load nxs
                 in loadImg mx

loadImg m = let w = M.width  m
                h = M.height m
                s = w*h
             in IT w h s (loadIT 1 1 w h s m H)

renderImg (IT w h s t) = wrapSVGTag w h (renderIT 0 0 w h t)

-- Auxiliares
nextPow2 n = nextPow2From n 2 
  where nextPow2From n p = if p>n then p else nextPow2From n (p*p)

pad s x xs = let l = length xs
              in xs ++ replicate (l-s) x

nextDir H = V
nextDir V = H

size :: ITree -> Int
size (S _)       = 1 
size (N _ t1 t2) = size t1 + size t2

compressIT :: ITree -> (Int, ITree)
compressIT (S c)     = (1, S c)
compressIT (N H t1 t2) = let (n1, tr1) = compressIT t1
                             (n2, tr2) = compressIT t2
                          in buildH n1 n2 tr1 tr2
compressIT (N V t1 t2) = let (n1, tr1) = compressIT t1
                             (n2, tr2) = compressIT t2
                          in buildV n1 n2 tr1 tr2

buildH :: Int -> Int -> ITree -> ITree -> (Int, ITree)
buildH _  _  (S c1) (S c2) = if c1==c2 then (1, S c1)
                                       else (2, N H (S c1) (S c2))
buildH n1 n2 t1     t2     = (n1+n2, N H t1 t2)

buildV :: Int -> Int -> ITree -> ITree -> (Int, ITree)
buildV _  _  (S c1) (S c2) = if c1==c2 then (1, S c1)
                                       else (2, N V (S c1) (S c2))
buildV n1 n2 t1     t2     = (n1+n2, N V t1 t2)

half n = n `div` 2

loadIT :: Width -> Height -> Width -> Height -> Size -> M.Matrix C.Color -> Dir -> ITree
loadIT iw ih fw fh 1 m _ = S (M.getAt m iw ih)
loadIT iw ih fw fh n m H = N H (loadIT iw           ih           fw (half fh) (half n) m V)
                               (loadIT iw           (ih+half fh) fw (half fh) (half n) m V)
loadIT iw ih fw fh n m V = N V (loadIT iw           ih           (half fw) fh (half n) m H)
                               (loadIT (iw+half fw) ih           (half fw) fh (half n) m H)

-- Auxiliaries for render
renderIT x y w h (S c)       = renderBlock x y w h c
renderIT x y w h (N H t1 t2) =  renderIT x y          w (half h) t1 
                             ++ renderIT x (y+half h) w (half h) t2
renderIT x y w h (N V t1 t2) =  renderIT x          y (half w) h t1  
                             ++ renderIT (x+half w) y (half w) h t2

wrapSVGTag:: Int -> Int -> String -> String
wrapSVGTag w h x = "<svg height=\"" ++ renderSize h ++ "\"" 
                    ++ " width=\""  ++ renderSize w ++ "\">"
                    ++ x ++ "\n</svg>"


renderBlock :: Int -> Int -> Int -> Int -> C.Color -> String
renderBlock x y w h c = 
     "\n<rect"
       ++ " x=\""      ++ renderSize x ++ "\"" 
       ++ " y=\""      ++ renderSize y ++ "\""
       ++ " width=\""  ++ renderSize w ++ "\""
       ++ " height=\"" ++ renderSize h ++ "\""
       ++ " style=\"fill:" ++ C.renderColor c ++ ";stroke-width:3;stroke:rgb(0,0,0)\"/>" 

unitSize     = 50
renderSize s = show (unitSize * s)
