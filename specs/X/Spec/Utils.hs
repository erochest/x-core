
module X.Spec.Utils where


import           Control.Applicative

import           Data.Hashable
import qualified Data.HashSet as S
import qualified Data.Text as T
import           Data.Time
import           Test.QuickCheck

import           X.Types


instance (Arbitrary a, Hashable a, Eq a) => Arbitrary (S.HashSet a) where
    arbitrary = S.fromList <$> arbitrary

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> arbitrary
instance Arbitrary DiffTime where
    arbitrary = secondsToDiffTime <$> arbitrary
instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary Priority where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary a => Arbitrary (Progress a) where
    arbitrary = Progress <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (ToDoStatus a) where
    arbitrary = oneof [ Active  <$> arbitrary
                      , Pending <$> arbitrary
                      , pure Someday
                      , Done    <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary T.Text where
    arbitrary = (T.pack <$>) . listOf1 . elements $ ' ' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

instance Arbitrary LinearEstimate where
    arbitrary = LE <$> arbitrary
instance Arbitrary FibEstimate where
    arbitrary = FE <$> arbitrary
instance Arbitrary ExpEstimate where
    arbitrary = EE <$> arbitrary

instance Arbitrary a => Arbitrary (ToDo a) where
    arbitrary =   ToDo
              <$> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> arbitrary
              <*> pure []

