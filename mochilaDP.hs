objetos=[(1,1),(4,3),(5,4),(7,5)]

wMax = 7

crearLista 0 = []
crearLista x = 0:(crearLista (x-1))

crearMatriz x 0 = []
crearMatriz x y = [(crearLista x)]++(crearMatriz x (y-1))

indexList i [] = error "seg fault"
indexList 0 (x:xs) = x
indexList j (x:xs) = indexList (j-1) xs

indexMatriz i j [] = error "seg fault"
indexMatriz 0 j (xs:xss)= indexList j xs
indexMatriz i j (xs:xss)= if (j<0) then error "rippeaste" else indexMatriz (i-1) j xss

reemplazarValor j [] a = error "seg fault"
reemplazarValor 0 (x:xs) a = a:xs
reemplazarValor j (x:xs) a = x:(reemplazarValor (j-1) xs a)

reemplazarEnMatriz i j [] a =  error "seg fault"
reemplazarEnMatriz 0 j (xs:xss) a = (reemplazarValor j xs a):xss
reemplazarEnMatriz i j (xs:xss) a = xs:(reemplazarEnMatriz (i-1) j xss a)

maximo a b = if (a > b) then a else b

dp [] wMax memo i j = foldr (maximo) 0 (map (foldr (maximo) 0) memo)
dp ((x,y):xs) wMax memo i j = if (j > wMax) then dp xs wMax memo (i+1) 0
                              else if (y>j) then dp ((x,y):xs) wMax (reemplazarEnMatriz i j memo (indexMatriz (i-1) j memo)) i (j+1)
                                   else dp ((x,y):xs) wMax (reemplazarEnMatriz i j memo (maximo (indexMatriz (i-1) j memo) ((indexMatriz (i-1) (j-y) memo)+x))) i (j+1)

problemaMochila wMax objetos = dp objetos wMax (crearMatriz (wMax+1) ((length objetos)+1)) 1 0
