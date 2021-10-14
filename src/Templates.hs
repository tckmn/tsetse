{-# LANGUAGE TemplateHaskell #-}

module Templates where

import Control.Monad
import Language.Haskell.TH

import Types
import Util

makeSetters :: Name -> DecsQ
makeSetters t = do
    TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify t
    forM fields $ \(name, _, _) ->
        head <$> [d|
            $(pure . VarP . mkName $ "set" ++ capName name) = \n ->
                GameIO $ \s g -> return (Right (), $(pure $ RecUpdE (VarE 'g) [ ( name, (VarE 'n) ) ] ) )
            |]
  where capName = capitalize . nameBase
