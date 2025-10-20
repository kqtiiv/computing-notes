[] :: [a] -- empty list
x:xs :: [a] -- a value x appended ont a list xs

-- head function is dangerous 

-- we could define
head [] = undefined

-- error function is similar 
error :: string -> a 