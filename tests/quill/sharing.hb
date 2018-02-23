--------------------------------------------------------------------------------
-- Quill prelude
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Primitive types and classes: functions and linearity

primitive type (-*>) :: * -> * -> *
primitive type (-!>) :: * -> * -> *
class (->) (f :: * -> * -> *)
infixr type 5 -*>, -!>, ->

instance (->) (-!>)
else (->) (-*>)
else (->) t fails

class t >:= u
instance t >:= u fails if Un t fails, Un u
else t >:= u

class Un t
instance Un (-!>)
instance Un ((-!>) t)
instance Un (t -!> u)

instance Un (-*>) fails
instance Un ((-*>) t) fails
instance Un (t -*> u) fails

class ShFun t
instance ShFun (-*>) fails
instance ShFun ((-*>) t) fails
instance ShFun (t -*> u) fails

class SeFun t
instance SeFun (-!>)
instance SeFun ((-!>) t)
instance SeFun (t -!> u)

--------------------------------------------------------------------------------

-- This means that a & b share resources
sPair = \a -> \&b -> \shp -> shp a b

-- This means a * b do not share resources
lPair = \c -> \*d -> \sep -> sep c d