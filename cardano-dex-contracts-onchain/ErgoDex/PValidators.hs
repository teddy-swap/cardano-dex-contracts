module ErgoDex.PValidators
  ( poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    validatorAddress,
    wrapValidator,
    writeValidator,
    writeValidatorUPLC,
    writeValidators,
    writeValidatorsTestnet,
    writeValidatorsUPLC
  )
where

import System.Directory (createDirectoryIfMissing)
import qualified Codec.CBOR.Write as Write
import qualified Data.ByteString.Base16 as Base16
import Data.Default (def)
import qualified Data.Text as Text
import qualified ErgoDex.PContracts.PDeposit as PD
import qualified ErgoDex.PContracts.PPool as PP
import qualified ErgoDex.PContracts.PRedeem as PR
import qualified ErgoDex.PContracts.PSwap as PS
import Plutarch
import Plutarch.Api.V2 (PValidator, mkValidator, validatorHash)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Internal
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import PlutusLedgerApi.V1.Address
import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import Ply.Plutarch

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as BSL
import System.IO (writeFile)

cfgForValidator :: Config
cfgForValidator = Config NoTracing

wrapValidator ::
  (PIsData dt, PIsData rdmr) =>
  Term s (dt :--> rdmr :--> PScriptContext :--> PBool) ->
  Term s (PData :--> PData :--> PScriptContext :--> POpaque)
wrapValidator validator = plam $ \datum redeemer ctx ->
  let dt = pfromData $ punsafeCoerce datum
      rdmr = pfromData $ punsafeCoerce redeemer
      result = validator # dt # rdmr # ctx
   in popaque $ pif result (pcon PUnit) (ptraceError "Validator reduced to False")

poolValidator :: Validator
poolValidator = mkValidator cfgForValidator $ wrapValidator $ PP.poolValidatorT teddyMagicNum

swapValidator :: Validator
swapValidator = mkValidator cfgForValidator $ wrapValidator $ PS.swapValidatorT teddyMagicNum

depositValidator :: Validator
depositValidator = mkValidator cfgForValidator $ wrapValidator $ PD.depositValidatorT teddyMagicNum

redeemValidator :: Validator
redeemValidator = mkValidator cfgForValidator $ wrapValidator $ PR.redeemValidatorT teddyMagicNum

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash

writeValidator ::
  forall cfg rdmr. (PIsData cfg, PIsData rdmr) =>
  (forall s. Term s (cfg :--> rdmr :--> PScriptContext :--> PBool)) ->
  String ->
  FilePath ->
  IO ()
writeValidator a description filename = 
  writeTypedScript def (Text.pack description) filename $ 
    wrapValidator @cfg @rdmr a


writeValidatorUPLC :: Validator -> FilePath -> IO ()
writeValidatorUPLC validator filename = do
    let bytes = Serialise.serialise validator
    BSL.writeFile filename bytes

teddyMagicNum :: Term s PInteger
teddyMagicNum = 5445445974

teddyMagicNumTestnet :: Term s PInteger
teddyMagicNumTestnet = 1111111;

writeValidators :: FilePath -> IO ()
writeValidators dir = do
    createDirectoryIfMissing True dir
    writeValidator (PP.poolValidatorT teddyMagicNum) "Pool Validator" (dir <> "/pool.plutus")
    writeValidator (PS.swapValidatorT teddyMagicNum) "Swap Validator" (dir <> "/swap.plutus")
    writeValidator (PD.depositValidatorT teddyMagicNum) "Deposit Validator" (dir <> "/deposit.plutus")
    writeValidator (PR.redeemValidatorT teddyMagicNum) "Redeem Validator" (dir <> "/redeem.plutus")

writeValidatorsTestnet :: FilePath -> IO ()
writeValidatorsTestnet dir = do
    createDirectoryIfMissing True dir
    writeValidator (PP.poolValidatorT teddyMagicNumTestnet) "Pool Validator" (dir <> "/pool.plutus")
    writeValidator (PS.swapValidatorT teddyMagicNumTestnet) "Swap Validator" (dir <> "/swap.plutus")
    writeValidator (PD.depositValidatorT teddyMagicNumTestnet) "Deposit Validator" (dir <> "/deposit.plutus")
    writeValidator (PR.redeemValidatorT teddyMagicNumTestnet) "Redeem Validator" (dir <> "/redeem.plutus")

writeValidatorsUPLC :: FilePath -> IO ()
writeValidatorsUPLC dir = do
    createDirectoryIfMissing True dir
    writeValidatorUPLC poolValidator (dir <> "/pool.uplc")
    writeValidatorUPLC swapValidator (dir <> "/swap.uplc")
    writeValidatorUPLC depositValidator (dir <> "/deposit.uplc")
    writeValidatorUPLC redeemValidator (dir <> "/redeem.uplc")