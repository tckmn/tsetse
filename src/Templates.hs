{-# LANGUAGE TemplateHaskell #-}

module Templates where

import Control.Monad
import Data.Char
import Language.Haskell.TH

import Types

fn :: String -> Name -> PatQ
fn s = pure . VarP . mkName . (s++) . capitalize . nameBase
    where capitalize "" = ""
          capitalize (c:s) = toUpper c : s

makeGameFns :: Name -> DecsQ
makeGameFns t = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify t

    foldr1 (liftM2 (++)) [[d|

        $(fn "get" name) = GameIO $ \s g ->
            return (Just ($(pure $ VarE name) g), g)

        $(fn "set" name) = \n -> GameIO $ \s g ->
            return (Just $([|n|]), $(pure $ RecUpdE (VarE 'g) [(name, VarE 'n)]))

        $(fn "mod" name) = \f -> GameIO $ \s g ->
            let r = f $ $(pure $ VarE name) g in
            return (Just r, $(pure $ RecUpdE (VarE 'g) [(name, VarE 'r)]))

      |] | (name, _, _) <- fields]

makeStateFns :: Name -> DecsQ
makeStateFns t = do
    TyConI (DataD _ _ _ _ [ForallC _ _ (RecC _ fields)] _) <- reify t

    foldr1 (liftM2 (++)) [[d|

        $(fn "get" name) = GameIO $ \s g ->
            return (Just ($(pure $ VarE name) s), g)

      |] | (name, _, _) <- fields, nameBase name /= "game"]

-- makeGameFns' :: Name -> DecsQ
-- makeGameFns' t = do
--     TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify t

--     foldr1 (liftM2 (++)) [[d|

--         $(fn "get" name) = \s g ->
--             return (Just ($(pure $ VarE name) g), g)

--         $(fn "set" name) = \n -> \s g ->
--             return (Just $([|n|]), $(pure $ RecUpdE (VarE 'g) [(name, VarE 'n)]))

--         $(fn "mod" name) = \f -> \s g ->
--             let r = f $ $(pure $ VarE name) g in
--             return (Just r, $(pure $ RecUpdE (VarE 'g) [(name, VarE 'r)]))

--       |] | (name, _, _) <- fields]
