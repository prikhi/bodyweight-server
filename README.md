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

* Settings
    * Document expected Environmental Variables
* User Auth
    * Protect Create/Update/Delete Routes
    * Tie Users into Other Models
    * When checking passwords against hashes, make sure the hash uses the latest hashing policy
* Format code like elm-format

## License

GPL-3.0
