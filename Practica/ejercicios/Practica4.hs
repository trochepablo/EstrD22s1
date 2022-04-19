data Pizza = Prepizza | Capa Ingrediente Pizza
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int

--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza    = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p

--Dada una lista de ingredientes construye una pizza
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

--Le saca los ingredientes que sean jamón a la pizza
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) =
    if esIngrediente ing Jamon
        then sacarJamon p
        else Capa ing (sacarJamon p)

esIngrediente :: Ingrediente -> Ingrediente -> Bool
esIngrediente Jamon         Jamon         = True
esIngrediente Queso         Queso         = True
esIngrediente Salsa         Salsa         = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _             _             = False

-- Dice si una pizza tiene salsa y queso
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso pizza = tieneIngrediente pizza Salsa && tieneIngrediente pizza Queso

tieneIngrediente :: Pizza -> Ingrediente -> Bool
tieneIngrediente Prepizza     _        = False
tieneIngrediente (Capa ing p) ingTiene = esIngrediente ing ingTiene || tieneIngrediente p ing


-- --Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = Capa (duplicarCantSiEsAceitunas ing) (duplicarAceitunas p)

duplicarCantSiEsAceitunas :: Ingrediente -> Ingrediente
duplicarCantSiEsAceitunas (Aceitunas cant) = Aceitunas (cant*2)
duplicarCantSiEsAceitunas ingrediente = ingrediente

-- -- Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
-- -- ingredientes de la pizza, y la respectiva pizza como segunda componente.
-- cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]