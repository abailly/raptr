-- | QuickCheck-based queue testing, using Monadic QuickCheck
--
-- see <http://www.cse.chalmers.se/~rjmh/Papers/QuickCheckST.ps Monadic Testing with QuickCheck> paper
-- for details.
module Network.Raptr.QueueSpec where

import           Control.Concurrent.Queue
import           Control.Concurrent.STM
import           Control.Monad            (when)
import           Control.Monad.Trans
import           GHC.Natural
import           Prelude                  hiding (take)
import qualified Prelude                  as P
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

data QAction = Put Int
    | Take
    | Flush
    | Return (Maybe Int)
    deriving (Eq, Show)

runActions :: Queue Int -> [ QAction ] -> STM [ Maybe Int ]
runActions q []            = return []
runActions q (Put i:as)    = put q i >> runActions q as
runActions q (Take:as)     = take q >>= \ i -> (i:) <$> runActions q as
runActions q (Flush:as)    = flush q >> runActions q as
runActions q (Return i:as) = (i:) <$> runActions q as


actions :: (Num a, Eq a) => a -> Gen [ QAction ]
actions n = oneof [ return []
                  , (:)
                    <$> (Put <$> arbitrary)
                    <*> actions (n+1)
                  , if n == 0
                    then return []
                    else (Take:) <$> actions (n-1)
                  , (Flush:) <$> actions 0
                  ]

delta :: [QAction] -> Int
delta q = delta' q 0
  where
    delta' [] n         = n
    delta' (Put _:qs) n = delta' qs (n+1)
    delta' (Take:qs) n  = delta' qs (n-1)
    delta' (Flush:qs) n = delta' qs 0

observe :: [QAction] -> [QAction] -> [QAction] -> PropertyM IO [Maybe Int]
observe pref actions suff = run $ atomically $ do
  q <- empty 100
  runActions q (pref ++ actions ++ suff)

-- | Observational equivalence of sequence of actions within arbitrary context
(==~) :: [ QAction ] -> [ QAction ] -> Property
a ==~ b = monadicIO $ do
  pref <- pick $ actions 0
  suff <- pick $ actions (delta $ pref ++ a)

  oc <- observe pref a suff
  oc' <- observe pref b suff

  assert $ oc == oc'

-- | Observational equivalence of sequence of actions with arbitrary suffix
(==^) :: [ QAction ] -> [ QAction ] -> Property
a ==^ b = monadicIO $ do
  suff <- pick $ actions (delta a)

  oc <- observe [] a suff
  oc' <- observe [] b suff

  assert $ oc == oc'

prop_put_take_yields_empty :: Int -> Property
prop_put_take_yields_empty n = [ Put n, Take ] ==^ [Return (Just n)]

prop_removes_fifo :: Int -> Int -> Property
prop_removes_fifo n m = [ Put n, Take, Put m ] ==~ [Put n, Put m, Take]

prop_flush_is_idempotent :: Property
prop_flush_is_idempotent = [Flush, Flush] ==~ [Flush]

instance Arbitrary Natural where
  arbitrary = do
    Positive p <- (arbitrary :: Gen (Positive Integer))
    pure $ fromInteger p

prop_put_fails_when_queue_is_full :: Natural -> Property
prop_put_fails_when_queue_is_full k = monadicIO $ do
  let n = fromInteger $ fromIntegral k
  suff <- pick $ actions k

  let observe o = run $ atomically $ do
        q <- empty k
        runActions q (P.take n (map Put [1..]) ++ o ++ suff)

  oc <- observe [ Put (n +1) ]
  oc' <- observe []

  assert $ oc == oc'


queueSpec :: Spec
queueSpec = do
  it "adding and removing yields empty q" $ property prop_put_take_yields_empty
  it "remove elements in FIFO order"      $ property prop_removes_fifo
  it "flush is idempotent"                $ property prop_flush_is_idempotent
  it "does not add more elements when queue full" $ property prop_put_fails_when_queue_is_full
