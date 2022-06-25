module Matrix(
    Matrix, new, load, width, height, getAt, setAt
) where

data Matrix a = M Int Int [[a]]
     deriving Show
  {- INV.REP.: en M w h rs
      * w = len rs > 0
      * para toda lista cs de rs. h = len cs > 0
  -}

new    :: Int -> Int -> a -> Matrix a
load   :: [[a]] -> Matrix a
width  :: Matrix a -> Int
height :: Matrix a -> Int
getAt  :: Matrix a -> Int -> Int      -> a
setAt  :: Matrix a -> Int -> Int -> a -> Matrix a

new w h x = let row = replicate h x
               in M w h (replicate w row)

load xss = let h = length xss
               w = length (head xss)
               allColumnsEqLen = all (\cs -> length cs == w) xss
            in if (h == 0) 
                then error "No rows"
                else if not allColumnsEqLen
                      then error "Columns of different sizes"
                      else M w h xss 

width  (M w _ _) = w
height (M _ h _) = h

getAt (M w h rs) i j =
    if 0 < i && i <= w && 0 < j && j <= h
     then (rs !! (j-1)) !! (i-1)
     else error "Matrix out of range"

setAt (M w h rs) i j x =
    if 0 < i && i <= w && 0 < j && j <= h
     then M w h (updateRow rs 1 1 i j x)
     else error "Matrix out of range"

-- Auxiliaries
updateRow :: [[a]] -> Int -> Int -> Int -> Int -> a -> [[a]]
{- PRECOND:
     * 0 < ci <= i 
     * 0 < cj <= j 
     * len rs > i 
     * para todo cs en rs. len cs > j
-}
updateRow []     _  _  _ _ _ = error "Acceso inválido"
updateRow (r:rs) ci cj i j x =
    if ci==i then updateCol r cj j x : rs
             else r : updateRow rs (ci+1) cj i j x

updateCol :: [a] -> Int -> Int -> a -> [a]
{- PRECOND:
     * 0 < cj <= j 
     * len xs > j
-}
updateCol []     _  _ _ = error "Acceso inválido"
updateCol (x:xs) cj j y =
    if cj==j then y : xs
             else x : updateCol xs (cj+1) j y

