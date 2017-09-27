{-# OPTIONS_GHC -w #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-- | A module where tuple classes and instances are created up to 16-tuple using 'makeTupleRefs'.
-- The number of classes and instances can be changed by hiding import from this module
-- and calling 'makeTupleRefs' in an other module.
module Control.Reference.TupleInstances where

import Control.Reference.TH.Tuple

$(makeTupleRefs hsTupConf 16 16)
