class T x | -> x
  where f :: x 

g :: T 
g = f
{-
class S x 
  where h :: x

j :: S
j = h
-}
class A x y | x -> y

i :: A T
i = g

j :: A t = b => b
j = g

k :: A t b => b
k = g

{-
l :: A t b c => c 
l = g
-}