module ErgoDex.Contracts.Proxy.Typed.Deposit where

import qualified Prelude as Haskell

import           Ledger
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import           ErgoDex.Contracts.Class
import           ErgoDex.Contracts.Types

data DepositConfig = DepositConfig
   { poolNft       :: Coin Nft
   , exFee         :: Amount Lovelace
   , rewardPkh     :: PubKeyHash
   , collateralAda :: Amount Lovelace
   } deriving stock (Haskell.Show)

instance UnliftErased DepositConfig D.DepositConfig where
  lift DepositConfig{..} = D.DepositConfig
    { poolNft       = unCoin poolNft
    , exFee         = unAmount exFee
    , rewardPkh     = rewardPkh
    , collateralAda = unAmount collateralAda
    }

  unlift D.DepositConfig{..} = DepositConfig
    { poolNft       = Coin poolNft
    , exFee         = Amount exFee
    , rewardPkh     = rewardPkh
    , collateralAda = Amount collateralAda
    }