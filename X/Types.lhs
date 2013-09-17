
> {-# LANGUAGE TemplateHaskell #-}

Introduction
============

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

The primary bit of data related to each todo item is its description.

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

The most complicated part of this is the state of the item. This type captures
the life-cycle-slash-state-machine nature of the todo item.

> data ToDoState = Someday
>                | Pending
>                | Active
>                | Done UTCTime
>                deriving (Show, Eq)

With all that in mind, the complete definition of the `ToDo` type is:

> data ToDo = ToDo
>           { _todoDescription :: T.Text
>           , _todoState       :: ToDoState
>           , _todoPriority    :: Maybe Priority
>           , _todoCreated     :: UTCTime
>           , _todoDue         :: Maybe UTCTime
>           , _todoTags        :: TagSet
>           , _todoContexts    :: ContextSet
>           , _todoUris        :: [URI]
>           } deriving (Show, Eq)
> makeLenses ''ToDo

<blockquote>
**TODO**: There needs to be some way of estimating and tracking progress. I'd
like to be able to use an exponential or Fibonacci scale for the estimate,
but a linear scale for the progress. Then I'd need to be able to translate
from one to the other. Seems like I can leverage some standard type classes
for this, but I need to figure out which ones. So for example, `F 6` would be
the sixth number in the Fibonacci sequence, or `L 8`, and `E 6` would be
$2^6$ or `L 64`. The difficult part would be translating the other way. `L 6`
would be slightly more than `F 5` and something like `E (5/2)`. Actually,
it's more complicated $2^{13/5}$ is quite a bit closer, though still not
exactly right. </blockquote>

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

[utctime]: http://hackage.haskell.org/packages/archive/time/latest/doc/html/Data-Time-Clock.html

