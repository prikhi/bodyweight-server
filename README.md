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
    * Add User/Subscriber field to RoutineLogs
    * When checking passwords against hashes, make sure the hash uses the
      latest hashing policy(and update it if it doesn't)
* Refactor all the `lift $ throwE` calls into a `serverError` function
    * Add some helpers for common  codes, like `notFound` & `forbidden`
    * Maybe rename `Types` module to `Server` and add these helper functions?
* Format code like elm-format
    * let/in & case-of expression newlines
    * multiline lists, tuples
    * multiline type definitions, `=>` & `->` line up w/ `::`
    * newline & 4 spaces after function name & args
    * don't care about alignment of imports or record types - or use
      autoformatter for those
    * `do` on line before the first statement in the `do`-block
* Add Markdown Docs using servant-doc.

## License

GPL-3.0
