module Stardust.IO where

import Prelude

import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Data.Maybe (fromMaybe)
import Effect.Aff.Class (class MonadAff, liftAff)

-- A bidirectional data stream
data Port m = Port (m Int) (Int -> m Unit)

-- Devices have multiple data streams and 
-- have the ability to interrupt the VM
data Device m = Device (Array (Port m)) (m Boolean)

-- Create a port from an input and output queue 
fromQueues :: forall m. MonadAff m => Queue Int -> Queue Int -> Port m
fromQueues input output = Port read write
  where
    read = liftAff $ Queue.tryRead input <#> fromMaybe 0
    write value = liftAff $ Queue.write output value
