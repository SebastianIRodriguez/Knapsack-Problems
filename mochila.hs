

sumaprecio b = foldr sum2precio 0 b 

sum2precio (a,b) c = b+c

maxl a b = if (sumaprecio a) < (sumaprecio b) then b else a

mochila [] w = []
mochila ((x,y):xs) w  = if ((w-x)<0) then (mochila xs w) 
    else maxl ((x, y):(mochila xs (w-x))) (mochila xs w)
