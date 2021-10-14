{-# LANGUAGE TemplateHaskell #-}

module ServerTemplates where

import Types
import Templates

$(makeStateFns ''ServerState)
