import Data.Matrix

objetos::[(Int,Int)]
objetos=[(1,1),(4,3),(5,4),(7,5)]

wMax::Int
wMax = 7

dp:: [(Int,Int)] -> Int -> Matrix Int -> Int -> Int -> Int
dp [] wMax memo i j = getElem (i-1) (wMax+1) memo
dp ((x,y):xs) wMax memo i j = if (j > (wMax+1) ) then dp xs wMax memo (i+1) 1
                              else if (y>(j-1)) then dp ((x,y):xs) wMax (setElem (getElem (i-1) j memo) (i, j) memo ) i (j+1)
                              else dp ((x,y):xs) wMax (setElem (max (getElem (i-1) j memo) ((getElem (i-1) (j-y) memo)+x)) (i, j) memo ) i (j+1)

problemaMochila :: [(Int,Int)] -> Int -> Int
problemaMochila objetos wMax = dp objetos wMax (matrix ((length objetos)+1) (wMax+1) (\(i,j) -> 0)) 2 1






