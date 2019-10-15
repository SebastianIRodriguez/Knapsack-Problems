-- GREEDY ( ( S , I ) , w)
-- A ← ∅
-- Ordenamos S en forma decreciente por pesos
-- para cada x ∈ S , tomados en orden decreciente de pesos
-- if A ∪ {x} ∈ I then
-- A ← A ∪ {x}
-- return A

knapsack list maxW = greedy (mergeSort list) maxW

-- Calcular el valor maximo que puede cargar la mochila
-- ::[(Double, Double)]		Lista de los productos con sus respectivos valores y pesos
-- ->Double					Peso maximo que puede cargar la mochila
-- ->Double					Valor maximo que puede cargar la mochila
greedy::[(Double, Double)] -> Double -> Double
greedy [] max_w = 0.0
greedy ((v, w):xs) max_w = if(max_w - w) > 0 
    then v + greedy xs (max_w - w)
    else v * max_w / w


mergeSort [] = []
mergeSort [a] = [a]
mergeSort a =
  merge (mergeSort firstFew) (mergeSort lastFew)
    where firstFew = take ((length a) `div` 2) a
          lastFew = drop ((length a) `div` 2) a
-- Expects a and b to already be sorted
merge a [] = a
merge [] b = b
merge ((a, b):as) ((c, d):bs)
  | (a/b) > (c/d) = (a, b):(merge as ((c, d):bs))
  | otherwise = (c, d):(merge ((a, b):as) bs)

a::[(Double, Double)]
a = [ 
    (60.0, 10.0),
    (100.0, 20.0),
    (120.0, 30.0)
    ]
c::[(Double, Double)]

c = [
  (36.0, 3.8),
  (43.0, 5.4),
  (90.0, 3.6),
  (45.0, 2.4),
  (30.0, 4.0),
  (56.0, 2.5),
  (67.0, 3.7),
  (95.0, 3.0),
  (98.0, 5.9)
  ]

b::Double
b = 15.0


