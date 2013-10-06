{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE RankNTypes        #-}


module X.TypesSpec where


import           Control.Monad
import qualified Data.HashSet as S
import qualified Data.List as L
import           Data.Ord
import           Data.Time

import           Control.Lens
import           Test.Hspec
import           Test.QuickCheck

import           X.Types
import           X.Spec.Utils ()

import Debug.Trace

traceTee :: Show a => String -> a -> a
traceTee m x = trace (m ++ ": '" ++ show x ++ "'") x


testSort :: (Ord a, Show a) => [a] -> [a] -> Expectation
testSort input = shouldBe (L.sort input)

testCompare :: Ord a => [(a, a, Ordering)] -> Expectation
testCompare = mapM_ cmp
    where cmp (a, b, c) = compare a b `shouldBe` c

type ToDoL  = ToDo LinearEstimate
type CopyFn = ToDoL -> ToDoL -> ToDoL

shouldSortOn :: Ord a
             => (a -> a -> Bool)
             -> (ToDoL -> a)
             -> [CopyFn]
             -> ToDoL
             -> ToDoL
             -> Bool
shouldSortOn cmp f ccs a b = (f x) `cmp` (f y)
    where (a', b') = propagateFields a b ccs
          [x, y]   = L.sort [a', b']

data Hole = Hole

propagateFields :: ToDoL -> ToDoL -> [CopyFn] -> (ToDoL, ToDoL)
propagateFields from to = (from,) . L.foldl' (trans' from) to
    where trans' a b f = f a b

transField :: Lens' ToDoL a -> ToDoL -> ToDoL -> ToDoL
transField lens from = set lens (from ^. lens)

compareLE :: (a -> a -> Ordering) -> a -> a -> Bool
compareLE f a b = GT /= f a b

spec :: Spec
spec = describe "ToDo" $ do
    describe "instance Ord" $ do
        it "should show active items first." $ do
            now <- getCurrentTime
            let a     = ToDo "a" (Active (Progress (LE 1) (FE 10))) Nothing now Nothing S.empty S.empty []
                b     = ToDo "b" (Pending (Progress (LE 0) (FE 3))) Nothing now Nothing S.empty S.empty []
                c     = ToDo "c" Someday Nothing now Nothing S.empty S.empty []
                d     = ToDo "d" (Done (Progress (LE 20) (FE 3)) now) Nothing now Nothing S.empty S.empty []
                tests = [ (a, a, EQ), (a, b, LT), (a, c, LT), (a, d, LT)
                        , (b, a, GT), (b, b, EQ), (b, c, LT), (b, d, LT)
                        , (c, a, GT), (c, b, GT), (c, c, EQ), (c, d, LT)
                        , (d, a, GT), (d, b, GT), (d, c, GT), (d, d, EQ)
                        ]
            testSort [d, c, b, a] [a, b, c, d]
            testCompare tests

        it "should show higher priority items first." $ do
            now <- getCurrentTime
            let a     = ToDo "a" Someday (Just A) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                b     = ToDo "b" Someday (Just B) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                c     = ToDo "c" Someday (Just C) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                d     = ToDo "d" Someday (Just D) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                e     = ToDo "e" Someday (Just E) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                tests = [ (a, a, EQ), (a, b, LT), (a, c, LT), (a, d, LT), (a, e, LT)
                        , (b, a, GT), (b, b, EQ), (b, c, LT), (b, d, LT), (b, e, LT)
                        , (c, a, GT), (c, b, GT), (c, c, EQ), (c, d, LT), (c, e, LT)
                        , (d, a, GT), (d, b, GT), (d, c, GT), (d, d, EQ), (d, e, LT)
                        , (e, a, GT), (e, b, GT), (e, c, GT), (e, d, GT), (e, e, EQ)
                        ]
            testSort [c, b, d, a, e] [a, b, c, d, e]
            testCompare tests

        it "should sort items with any priority before no-priority items." $ do
            now <- getCurrentTime
            let a     = ToDo "a" Someday (Just A) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                b     = ToDo "b" Someday (Just B) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                c     = ToDo "c" Someday (Just C) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                d     = ToDo "d" Someday (Just D) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                e     = ToDo "e" Someday (Just E) now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                n     = ToDo "n" Someday Nothing now Nothing S.empty S.empty []  :: ToDo LinearEstimate
                tests = [ (a, n, LT), (b, n, LT), (c, n, LT), (d, n, LT), (e, n, LT)
                        , (n, a, GT), (n, b, GT), (n, c, GT), (n, d, GT), (n, e, GT)
                        ]
            testSort [c, b, d, n, a, e] [a, b, c, d, e, n]
            testCompare tests

        it "should sort ones with a due date first." $ do
            now <- getCurrentTime
            let today    = utctDay now
                tmrw     = addDays 1 today
                nextWeek = addDays 7 today
                a        = ToDo "a" Someday Nothing now (Just today)    S.empty S.empty [] :: ToDo LinearEstimate
                b        = ToDo "b" Someday Nothing now (Just tmrw)     S.empty S.empty [] :: ToDo LinearEstimate
                c        = ToDo "c" Someday Nothing now (Just nextWeek) S.empty S.empty [] :: ToDo LinearEstimate
                d        = ToDo "d" Someday Nothing now Nothing         S.empty S.empty [] :: ToDo LinearEstimate
                tests    = [ (a, a, EQ), (a, b, LT), (a, c, LT), (a, d, LT)
                           , (b, a, GT), (b, b, EQ), (b, c, LT), (b, d, LT)
                           , (c, a, GT), (c, b, GT), (c, c, EQ), (c, d, LT)
                           , (d, a, GT), (d, b, GT), (d, c, GT), (d, d, EQ)
                           ]
            testSort [c, b, d, a] [a, b, c, d]
            testCompare tests

        it "should sort by description." $ do
            now <- getCurrentTime
            let a = ToDo "a" Someday Nothing now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                b = ToDo "b" Someday Nothing now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                c = ToDo "c" Someday Nothing now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                d = ToDo "d" Someday Nothing now Nothing S.empty S.empty [] :: ToDo LinearEstimate
                tests = [ (a, a, EQ), (a, b, LT), (a, c, LT), (a, d, LT)
                        , (b, a, GT), (b, b, EQ), (b, c, LT), (b, d, LT)
                        , (c, a, GT), (c, b, GT), (c, c, EQ), (c, d, LT)
                        , (d, a, GT), (d, b, GT), (d, c, GT), (d, d, EQ)
                        ]
            testSort [c, d, b, a] [a, b, c, d]
            testCompare tests

        it "should sort on status (quickcheck)."      $ property $
            shouldSortOn (<=) _todoStatus []
        it "should sort on priority (quickcheck)."    $ property $
            shouldSortOn (compareLE reorderMaybe) _todoPriority [transField todoStatus]
        it "should sort on due date (quickcheck)."    $ property $
            shouldSortOn (compareLE reorderMaybe) _todoDue [ transField todoStatus
                                                           , transField todoPriority]
        it "should sort on description (quickcheck)." $ property $
            shouldSortOn (<=) _todoDescription [ transField todoStatus
                                               , transField todoPriority
                                               , transField todoDue
                                               ]

