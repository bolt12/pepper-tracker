
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
`scotty` for the webserver and ~~`blaze`~~ `lucid` on the frontend.

The things to learn with this phase are:

  - Learn the hassles of doing everything in-memory without the abstraction/help
    of database;

  - How to use `scotty`, nothing fancy here;

  - How to use `blaze`, and how it interacts with `scotty`.

#### Conclusions

- To run the DB in memory I needed to define all the CRUD operations on the
  datatypes (Tables). Since the project has very little functionalities I ended
  up by just keeping it simple but I can see how this could scale with a larger
  problem domain. It's not worth it to implement your own query system. Just use
  a standard DB.

- To be able to run it in memory with scotty was easy by following an example
  from the docs. It uses a ScottyT which takes an monadic action and executes it
  everytime a request comes. This monadic action just runs one time per action so the state
  would be lost with a standard State monad; the trick was to use TVar from STM
  that allocates a shared memory location that support atomic memory
  transactions and save the ReaderT in that location. The monadic action simply
  reads the value of the TVar. It was a very nice and simple exercise to see STM
  in action.

- Scotty is very simple and intuitive so designing the route endpoints was
  also very simple.

- I switched from `blaze` to `lucid` for HTML generation because I found
  `blaze` a little rough around the edges and was not very friendly to use. This
  [link](https://chrisdone.com/posts/lucid/) explains better the problems with
  `blaze` and shows how `lucid` fixes them.

- The first phase is a little rough around the edges mainly because it is very
  tedious to keep up with all the possible CRUD operations + supporting Views.
  With this being said, I conclude that with such a simple example, working with
  an in-memory DB and doing all the dirty work is painful and bug prone. The
  `lucid` HTML generation is fun to do but since it supports limited interaction
  with javascript it does not allow to have an user-friendly interface.
  Regrading `scotty`, it's very pleasant to work with and I should have
  organized the routes a little better since the number of endpoints can scale
  when doing server-side rendering. 

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

## Persistent file storage format for phase 1

We'll call every data type defined above a table in our "DB" except the `Form`
because it's too simple and we're going to see it as a simple enum type. We
could save everything on one file but that is going to be harder to parse. An
easier approach is to have one file per table and have a specific parser to each
one.

The format isn't going to be anything fancy. It'll be used the `Show` instance
of each type to serialize them and the `Read` instance to read it back. Every
update will be done in-memory and then saved on the file.

The `Show` instance is easily derived by the compiler, but the parser for the
`Read` instance will have to be written by hand. That's where `megaparsec`
comes.
