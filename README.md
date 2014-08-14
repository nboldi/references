References can read, write or update parts of the data.
They are first-class values, can be passed in functions, transformed, combined.
References generalize lenses, folds and traversals for haskell (see: https://hackage.haskell.org/package/lens).

There are two things that references can do but the previously mentioned access methods don't.
 * References can cooperate with monads, for example IO.
 * References can be added using the `&+&` operator, to create new lenses more easily.

Basic idea taken from the currently not maintained package https://hackage.haskell.org/package/yall. 

An example use of the references (a logger application that spawns new threads to update a global log):

```haskell
logger =
  (forever $ do
     log <- logChan ^! chan&logRecord    -- Extract the log record from the received log message
     thrId <- forkIO (do time <- getTime
                         ioref&lastLogTime != time $ logDB     -- Update the last logging time mutable log database
                         let logMsg = senderThread .- show     -- Transform the thread id to a string and
                                        $ loggingTime .= time  -- update the time
                                        $ log                  -- inside the log message
                         ioref&debugInfos !~ addLogEntry log $ logDB  -- update the table of log entries
                         mvar !- (+1) $ count )
     mvar !- (thrId:) $ updaters                               -- Record the spawned thread
    ) `catch` stopUpdaters updaters
  where stopUpdaters updaters ThreadKilled =    
          mvar&traverse *!| killThread $ updaters              -- Kill all spawned threads before stopping
```

There are a bunch of predefined references for datatypes included in standard libraries.

New references can be created in several ways:
 * From getter, setter and updater, using the `reference` function.
 * From getter and setter, using one of the simplified functions (`lens`, `simplePartial`, `partial`, ...).
 * Using the `Data.Traversal` instance on a datatype to generate a traversal of each element.
 * Using lenses from `Control.Lens` package. There are a lot of packages defining lenses, folds and traversals
   for various data structures, so it is very useful that all of them can simply be converted into a reference.
 * Generating references for newly defined records using the `makeReferences` Template Haskell function.



