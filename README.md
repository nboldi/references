References are data accessors that can read, write or update the accessed infromation through their context.
They are first-class values, can be passed in functions, transformed, combined.
References generalize lenses, folds and traversals for haskell (see: https://hackage.haskell.org/package/lens).

References are more general than field selectors in traditional languages.
 * References are first-class values. If there is a struct in C, for example, with an `int` field `fl`, then fl can only be used as part of an expression. One can not generalize a function to take a field selector and transform the selected data or use it in other ways.
 * They can have different meanings, while field accessors can only represent data-level containment. They can express uncertain containment (like field selectors of C unions), different viewpoints of the same data, and other concepts.

References are more potent than lenses, folds and traversals:
 * References can cooperate with monads, for example IO. This opens many new applications.
 * References can be added using the `&+&` operator, to create new lenses more easily.

Basic idea taken from the currently not maintained package https://hackage.haskell.org/package/yall. 

An example use of the references (a logger application that spawns new threads to update a global log):

```haskell
logger =
  (forever $ do
     log <- logChan ^? chan&logRecord    -- Extract the log record from the received log message
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
          mvar&traverse !| killThread $ updaters               -- Kill all spawned threads before stopping
```

There are a number of predefined references for datatypes included in standard libraries.

New references can be created in several ways:
 * From getter, setter and updater, using the `reference` function.
 * From getter and setter, using one of the simplified functions (`lens`, `simplePartial`, `partial`, ...).
 * Using the `Data.Traversal` instance on a datatype to generate a traversal of each element.
 * Using lenses from `Control.Lens` package. There are a lot of packages defining lenses, folds and traversals for various data structures, so it is very useful that all of them can simply be converted into a reference.
 * Generating references for newly defined datatypes using the `makeReferences` Template Haskell function.

[![Hackage](https://img.shields.io/hackage/v/references.svg?maxAge=2592000)]()

