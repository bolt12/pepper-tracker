
## Pepper Tracker project

Pepper Tracker is a simple personal project written in Haskell. It keeps track
of what peppers I am planting or have planted and keeps track of the different
hot sauces I tried with them.

This project will have 3 approaches. Since I'm still new to the web ecossystem in
Haskell I want to experiment with different approaches and learn with them.
Given this, this repository will hold all the 3 approaches documented for
everyone that is still new to this Haskell ecossystem and wants to see how a
simple project can be done.

## The phases

All phases will, more or less, be structured using the 3 layer pattern. The idea
here is not to use the best/fastest/bleeding edge technologies/frameworks, but I
also don't want to use some framework that pretty much everything for me. It's
not ideal to spend most of the time looking at the docs of some library.
Probably the solutions presented here won't be the best or use the best
practices for performance and that is a good thing because that's how we learn!

Future work will focus on improving the solutions by refactoring.

### First phase

The first phase is the most simple, naive way of doing things. It'll be done by
using an in-memory DB with a file persistent storage, for the data layer;
`scotty` for the webserver and `blaze` on the frontend.

The things to learn with this phase are:

  - Parsing of the file persistent storage, with `megaparsec`;

  - Learn the hassles of doing everything in-memory without the abstraction/help
    of database;

  - How to use `scotty`, nothing fancy here;

  - How to use `blaze`, and how it interacts with `scotty`.

#### Conclusions

### Second phase

The second phase aims to change the data layer and use a database management
system like Postgresql. It will use `persistent` + `esqueleto` to interface with
the DB and hopefully everything relevant will be taken care of by them.

The things to learn with this phase are:

  - How to use `persistent` + `equeleto` and their limitations. That is see if
    they are fit to this type of projects; I don't know how will they handle
    foreign keys, probably because I haven't read the docs...;

  - How easy it is to refactor the first phase code wrt the data layer;

#### Conclusions

### Third phase

The third phase is a little more ambitious and I haven't decided how what I'm
going to do. I have a couple of options:

  - Change the webserver from `scotty` to `servant` and see how it fits with
    this project and how a more type safe approach feels like in webserver
    development;

  - Try and scale the app to multiple users and see how much of an hassle it is
    to implement authentication;

  - Decouple the frontend using Elm (not Haskell) or Reflex (Haskell).

#### Conclusions

## Schema

Here I'll present the schema that I will use in simple Haskell syntax.

As I said in the beginning I want to keep track of which hot peppers I've grown
and what types of hot sauces I've tried. With this being said:

```Haskell
data Pepper = Pepper {
  pepperId :: Int,
  name :: String,
  scoville :: Int,
  planted :: UTCTime,
  firstFruit :: UTCTime,
  rating :: Int,
  active :: Bool
}

type HotSauceId = Int
type PepperId = Int

type Kg = Float
type Month = Int

data Form = Fermented Month | Raw | Roasted

data HotSauce = HotSauce {
  hotSauceId :: HotSauceId,
  peppers :: [(PepperId, Kg, Form)],
  rating :: Int 
}
```

The only restriction here is the `rating` attribute in both data structures
where the maximum value is 10 and the `PepperId` should map to an existing
pepper in the DB.
