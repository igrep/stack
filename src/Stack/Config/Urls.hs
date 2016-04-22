{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Stack.Config.Urls
    ( urlsFromMonoid
    , askLatestSnapshotUrl
    ) where

import           Stack.Types
import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.Logger (MonadLogger, logWarn)
import           Control.Monad.Reader (MonadReader, asks)
import           Data.Maybe
import           Data.Text (Text)

urlsFromMonoid :: MonadLogger m => Maybe Text -> UrlsMonoid -> m Urls
urlsFromMonoid maybeLatestSnapshotUrl monoid = do
    when (isJust maybeLatestSnapshotUrl) $ do
        $logWarn "WARNING: latest-snapshot-url is deprecated. Use 'latest-snapshot' in 'urls' instead."

    return $ Urls
        (fromMaybe defaultLatestSnapshot    $ urlsMonoidLatestSnapshot    monoid <|> maybeLatestSnapshotUrl)
        (fromMaybe defaultLtsBuildPlans     $ urlsMonoidLtsBuildPlans     monoid)
        (fromMaybe defaultNightlyBuildPlans $ urlsMonoidNightlyBuildPlans monoid)
    where
    defaultLatestSnapshot =
        "https://www.stackage.org/download/snapshots.json"
    defaultLtsBuildPlans =
        "https://raw.githubusercontent.com/fpco/lts-haskell/master/"
    defaultNightlyBuildPlans =
        "https://raw.githubusercontent.com/fpco/stackage-nightly/master/"

-- | Get the URL to request the information on the latest snapshots
askLatestSnapshotUrl :: (MonadReader env m, HasConfig env) => m Text
askLatestSnapshotUrl = asks (urlsLatestSnapshot . configUrls . getConfig)
