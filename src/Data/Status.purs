module Data.Status
  ( class Status
  , report
  , reportError
  )
  where

import Data.Operator.Bottom (class Bottom2, bottom2)
import Data.Operator.Top (class Top1_, top1_)
import Type.Proxy (Proxy)

class Status f a where
  report :: forall b. Proxy a -> b -> f b
  reportError :: forall b. a -> f b

instance statusBottom1_Top1_ :: (Bottom2 f a, Top1_ f) => Status f a where
  report _ = top1_
  reportError = bottom2
