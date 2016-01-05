module Network.Raptr.StorageSpec where

import           Control.Exception       (bracket)
import           Control.Monad.Trans     (liftIO)
import           Data.Binary
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Network.Raptr.Raptr     hiding (run)
import           Network.Raptr.TestUtils
import           System.IO.Storage
import           System.Random
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

newtype Payload = P Int deriving (Eq, Show)

instance Binary Payload where
  put (P i) = put i
  get       = P <$> get

instance Arbitrary Payload where
  arbitrary = P <$> arbitrary

data SAction = AddEntry Payload
             | Truncate Int
             | GetEntry Int
             | LastEntry
             | Observe (Maybe Payload)
             deriving (Eq,Show)

payloadFromEntry = fmap (decode . fromStrict . eValue)

indexAt i = foldr (const succIndex) index0 [1 .. i]

runActions :: FileLog -> [ SAction ] -> IO [ Maybe Payload ]
runActions log []              = return []
runActions log (Observe p:as)  = (p:) <$> runActions log as
runActions log (Truncate i:as) = truncateLogAtIndex log idx >> runActions log as
  where
    idx = indexAt i
runActions log (AddEntry s:as) = insertEntry log e >> runActions log as
  where
    e   = Entry index0 term0 (toStrict $ encode s)
runActions log (GetEntry i:as) = decodeEntry >>= \ e -> (e:) <$> runActions log as
  where
    idx = indexAt i
    decodeEntry = do
      e <- getEntry log idx
      return $ payloadFromEntry e
runActions log (LastEntry:as) = decodeLastEntry >>= \ e -> (e:) <$> runActions log as
  where
    decodeLastEntry = do
      e <- getLastEntry log
      return $ payloadFromEntry e


actions :: Int  -- ^ length of underlying structure
           -> Gen [ SAction ]
actions n = oneof [ return []
                  , (:)                 <$>
                    (AddEntry <$> arbitrary) <*>
                    actions (n+1)
                  , do
                      i <- choose (1,n)
                      (GetEntry i:) <$> actions n
                  , (LastEntry:) <$> actions n
                  , if n == 0
                    then return []
                    else do
                      i <- choose (0, n)
                      (Truncate i:) <$> actions i
                  ]


delta :: [SAction] -> Int
delta q = delta' q 0
  where
    delta' [] n = n
    delta' (AddEntry _:qs) n = delta' qs (n+1)
    delta' (Truncate i:qs) n = delta' qs i
    delta' (_:qs)          n = delta' qs n

observe :: [SAction] -> [SAction] -> [SAction] -> PropertyM IO [Maybe Payload]
observe pref actions suff = run $
                            bracket
                            (randomRIO (1,100000000 :: Int) >>= \ i-> openLog ("test-qc-" ++ show i ++ ".log") )
                            (removeFileSafely . logName)
                            (flip runActions (pref ++ actions ++ suff))

-- | Observational equivalence of sequence of actions within arbitrary context
(==~) :: [ SAction ] -> [ SAction ] -> Property
a ==~ b = monadicIO $ do
  pref <- pick $ actions 0
  suff <- pick $ actions (delta $ pref ++ a)

  oc <- observe pref a suff
  oc' <- observe pref b suff

  liftIO $ putStrLn $ "observing " ++ show oc ++ " for "  ++ show a
  liftIO$ putStrLn $ "observing " ++ show oc' ++ " for "  ++ show b

  assert $ oc == oc'

-- | Observational equivalence of sequence of actions with arbitrary prefix
(^==) :: [ SAction ] -> [ SAction ] -> Property
a ^== b = monadicIO $ do
  pref <- pick $ actions 0

  oc <- observe pref a []
  oc' <- observe pref b []

  liftIO $ putStrLn $ "observing " ++ show oc ++ " for "  ++ show a
  liftIO$ putStrLn $ "observing " ++ show oc' ++ " for "  ++ show b

  assert $ oc == oc'



prop_last_entry :: Payload ->  Property
prop_last_entry n = [ AddEntry n, LastEntry ] ^== [ Truncate 0, AddEntry n, LastEntry ]

prop_last_entry_get_first_after_truncate :: Payload ->  Property
prop_last_entry_get_first_after_truncate n = [ Truncate 0, AddEntry n, GetEntry 1 ] ^== [ AddEntry n, LastEntry ]

prop_get_0_is_empty :: Payload ->  Property
prop_get_0_is_empty n = [ GetEntry 0 ] ==~ [ Observe Nothing ]

storageSpec :: Spec
storageSpec = do
  it "last entry always retrieves previously added entry" $ property prop_last_entry
  it "last entry equals get first entry after truncate"   $ property prop_last_entry_get_first_after_truncate
  it "entry 0 is always empty"                            $ property prop_get_0_is_empty
