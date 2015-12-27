-- | QuickCheck-based queue testing, using Monadic QuickCheck
--
-- see <http://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps Monadic Testing with QuickCheck> paper
-- for details.
module Network.Raptr.QueueSpec where

import           Control.Concurrent.Queue
import           Control.Concurrent.STM
import           Control.Monad.Trans
import           Prelude                  hiding (take)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

data QAction = Put Int
             | Take
             | Return Int
             deriving (Eq,Show)

runActions :: Queue Int -> [ QAction ] -> STM [ Int ]
runActions q []            = return []
runActions q (Put i:as)    = put q i >> runActions q as
runActions q (Take:as)     = take q >> runActions q as
runActions q (Return i:as) = (i :) <$> runActions q as


actions :: (Num a, Eq a) => a -> Gen [ QAction ]
actions n = oneof [ return []
                  , (:)
                    <$> (Put <$> arbitrary)
                    <*> actions (n+1)
                  , if n == 0
                    then return []
                    else (Take :) <$> actions (n-1)
                  ]

delta :: [QAction] -> Int
delta q = delta' q 0
  where
    delta' [] n = n
    delta' (Put _:qs) n = delta' qs (n+1)
    delta' (Take:qs) n  = delta' qs (n-1)

-- | Observational equivalence on sequence of actions over some queue
(==~) :: [ QAction ] -> [ QAction ] -> Property
a ==~ b = monadicIO $ do
  pref <- pick $ actions 0
  suff <- pick $ actions (delta $ pref ++ a)
  let observe v = run $ atomically $ do
        q <- empty 10
        runActions q (pref ++ v ++ suff)
  oc <- observe a
  oc' <- observe b

  assert $ oc == oc'

prop_add_remove_yields_empty :: Int -> Property
prop_add_remove_yields_empty n = [ Put n, Take ] ==~ []

prop_removes_fifo :: Int -> Int -> Property
prop_removes_fifo n m = [ Put n, Take, Put m ] ==~ [Put n, Put m, Take]

queueSpec :: Spec
queueSpec = do
  it "adding and removing yields empty q" $ property prop_add_remove_yields_empty
  it "remove elements in FIFO order" $ property prop_removes_fifo

