{-# LANGUAGE TemplateHaskell #-}

module Templates where

import Control.Monad
import Language.Haskell.TH

import Types
import Util

makeMonadFns :: Name -> DecsQ
makeMonadFns t = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify t

    foldr1 (liftM2 (++)) [[d|

        $(fn "get" name) = GameIO $ \s g ->
            return (Right ($(pure $ VarE name) g), g)

        $(fn "set" name) = \n -> GameIO $ \s g ->
            return (Right $([|n|]), $(pure $ RecUpdE (VarE 'g) [(name, VarE 'n)]))

        $(fn "mod" name) = \f -> GameIO $ \s g ->
            let r = f $ $(pure $ VarE name) g in
            return (Right r, $(pure $ RecUpdE (VarE 'g) [(name, VarE 'r)]))

      |] | (name, _, _) <- fields]

  where fn s = pure . VarP . mkName . (s++) . capitalize . nameBase
