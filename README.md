# Bodyweight Logger Server

A Bodyweight Workout Logging REST server written in Haskell.

Right now this is pretty much a minimal REST API wrapper around a postgresql
database.

CRUD operations on the following resources are exposed:

* Exercises
* Routines
* Sections/SectionExercises
* Subscriptions
* Users
* Routine Logs

## Setup

```
pacman -S stack
make
```

## TODO

* Documentation
    * Add expected Environmental Variables to README
    * Document Typeclasses & instance implementations in Models module
    * Add API Docs using servant-doc
* User Auth
    * Add User/Subscriber field to RoutineLogs
* Refactor all the `lift $ throwE` calls into a `serverError` function
    * Add some helpers for common  codes, like `notFound` & `forbidden`
    * Maybe rename `Types` module to `Server` and add these helper functions?

## Code Style

Loosely based off of Elm's style guide & `elm-format`, the goal is to minimize
noise in commit diffs.

Try not to go too far over a line width of 80 characters.

**Let/In/Where**

Use let/in for values & where for functions:

```
someFunc someArg =
    let
        someVar =
            someExpr 2 3 4
    in
        return $ wrap someVar
    where wrap =
            Var

someFunc someArg =
    extrapolate 2 3 4 someArg
    where extrapolate =
            anotherFunc 2 4
```

**Multiline Lists/Tuples**

```
[ firstElem
, secondElem
, thirdElem
]
```

**Multiline Type Definitions**

```
someFunction :: (ShortTypeclass a) => a -> a

someFunction :: ( TypeclassOne a b c, AVeryLongTypeclassNameSoWeCanShowWrapping a
                , TypeclassThree c )
             => a -> b -> c

someFunction :: SomeReallyLongFunctionArguement
             -> AndAnotherReallyLongOneSoWeCantFitItAllInALine
             -> Int
```

**Function Definitions**

```
someFunction arg1 arg2 arg3 =
    someExpr arg2 arg3 $ otherExpr arg1


someFunction someArg monadicFunction = do
    someResult <- monadicFunction
    handleResult someArg someResult

someFunction someArg =
    aMultilineFunctionCall withLots ofArguments thatWont fitIn
        aSingleLine
```

**Imports**

Proper order is explicit imports, qualified imports, application imports.
Alignment doesn't really matter, but we might automate it using
stylish-haskell in the future.

```
module MyModule where

import Data.Maybe (isJust, isNothing)
import Servant (err401)

import qualified Data.Text as T

import Models
import Types
```


## License

GPL-3.0
