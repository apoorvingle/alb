data T1 a b = L a | R b

data Bar t = MkBar1 t
           | MkBar2 t a b
           | MkBar3 t a forall a


-- Right (Datatype (At :1:6-10
--                  (TyApp (At :1:6-8 (TyCon (Ident "T" 0 Nothing)))
--                  (At :1:8-10 (TyVar (Ident "t" 0 Nothing)))))
--        [ Ctor {ctorName = At :1:12-16 (Ident "Mk1" 0 Nothing)
--              , ctorParams = []
--              , ctorQualifiers = []
--              , ctorFields = [At :1:16-18 (TyVar (Ident "a" 0 Nothing))]}
--        , Ctor {ctorName = At :1:20-24 (Ident "Mk2" 0 Nothing)
--               , ctorParams = []
--               , ctorQualifiers = []
--               , ctorFields = [ At :1:24-26 (TyVar (Ident "a" 0 Nothing))
--                              , At :1:26-28 (TyVar (Ident "b" 0 Nothing))]}
--        , Ctor {ctorName = At :1:30-34 (Ident "Mk3" 0 Nothing)
--               , ctorParams = []
--               , ctorQualifiers = []
--               , ctorFields = [ At :1:34-36 (TyVar (Ident "a" 0 Nothing))
--                              , At :1:36-38 (TyVar (Ident "b" 0 Nothing))]}]
--        []
--        Nothing)
