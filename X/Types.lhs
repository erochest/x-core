% X.Types
% Eric Rochester
%

Introduction
============

> {-# LANGUAGE TemplateHaskell #-}
>
> module X.Types where
>
> import           Data.Time
>
> import           Control.Lens hiding (Context)
> import qualified Control.Lens as Lens
> import qualified Data.HashSet as S
> import qualified Data.Text as T
> import           Text.URI

This library encompasses a number of different funcations.

* To do list;
* Time tracking;
* Outliner; and
* Notes (unhierarchical).

They're all related (productivity software), but also full data models in their
own rights. I've attempted to both treat each separately, as far as it makes
sense, but also to all them to interoperate where it would be helpful to do so.

To Do List
----------

One of the more complex models in this is the model for the todo list.

The primary bit of data related to each todo item is its description. This is
just a text field.

Each todo can optionally also have a priority. At the moment, the values for
this are a hard-coded-enum-type. The values are listed in reverse alphabetical
order, because in this, `A` is "larger" than `E`, which is the minimal value.
Ordering it this way should make sorting easier and more intuitive.

Some other options for this data type may involve a newtype over a Int, Char,
or other ordered type. This could then still derive the same set of type
classes as it does here. But what I am currently doing has the benefit of being
more type-safe. For example, given a `Priority` constructor `P`, it's difficult
to say exactly what `P (-42)` or `P '*'` mean (or even `P 'k'`, if `P 'j'` and
others aren't defined).

> data Priority = E | D | C | B | A
>         deriving (Show, Eq, Ord, Enum)

There are a number of time fields---`todoCreated`, for example---and all are
stored internally as [`Data.Time.UTCTime`][utctime]. They should be converted
to local time before being displayed and immediately after being entered.

Tags are simply a set of `T.Text`. Personally, I use this to relate them to one
or more projects, but being tags, their semantics are infinitely variable.

> type Tag    = T.Text
> type TagSet = S.HashSet Tag

I use a similar structure for contexts, but since their semantics are
different, I've created a different type alias for them. It is possible that
the definition for tags and contexts may diverge at some point, and
distinguishing them now will make this possible.

> type Context    = T.Text
> type ContextSet = S.HashSet Context

But the most complicated part of all of this, for a different kind of
complexity, is the estimate notation. This can handle several different types
of estimates: linear, Fibonacci, and exponential. They all can be converted to
integral types by "downcasting" to a linear scale. Therefore, estimates can be
in any integral type; however, progress tracking is only done with a linear
scale. Therefore, the only useful, valid view of the data involves graphically
displaying the amount done using a progress bar.

> data LinearEstimate = LE Int
>         deriving (Show, Eq, Ord)
> data FibEstimate    = FE Int
>         deriving (Show, Eq, Ord)
> data ExpEstimate    = EE Int
>         deriving (Show, Eq, Ord)

All of these are instances of an `Estimate` type class. Instances of this class
just need to be able to convert themselves into a `LinearEstimate`.

> class Estimate e where
>     toLinear :: e -> LinearEstimate
>
> instance Estimate LinearEstimate where
>     toLinear = id
> instance Estimate FibEstimate where
>     toLinear (FE n) = LE $ fibs !! n
> instance Estimate ExpEstimate where
>     toLinear (EE n) = LE . round . (2.0 **) $ fromIntegral n

This is a potential memory leak, since we're never releasing the full cache 

> fibs :: [Int]
> fibs = ([1, 1] ++) . zipWith (+) fibs $ drop 1 fibs

However, estimates aren't just for planning. They're also for tracking
progress. For that, we'll use a `Progress` type that includes the `Estimate`
for the projection and a `LinearEstimate` for the progress.

> data Progress e = Progress
>                 { _progress :: LinearEstimate
>                 , _estimate :: e
>                 } deriving (Show, Eq)
> makeLenses ''Progress

The progress is tracked in the status of a `ToDo` item as it moves through its
life cycle.

> data ToDoStatus e = Someday
>                   | Pending (Progress e)
>                   | Active (Progress e)
>                   | Done (Progress e) UTCTime
>                   deriving (Show, Eq)

With all that in mind, the complete definition of the `ToDo` type is:

> data ToDo e = ToDo
>             { _todoDescription :: T.Text
>             , _todoStatus      :: ToDoStatus e
>             , _todoPriority    :: Maybe Priority
>             , _todoCreated     :: UTCTime
>             , _todoDue         :: Maybe UTCTime
>             , _todoTags        :: TagSet
>             , _todoContexts    :: ContextSet
>             , _todoUris        :: [URI]
>             } deriving (Show, Eq)
> makeLenses ''ToDo

<blockquote>
**TODO**: I need to define `Ord` for the `ToDo` type that will order by
priority, state, due date, and description. Need specs to nail down the exact
semantics of this. </blockquote>

Time Tracking
-------------

Outliner
--------

Notes
-----

Bookmarks/Reading List
----------------------

[utctime]: http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Clock.html

