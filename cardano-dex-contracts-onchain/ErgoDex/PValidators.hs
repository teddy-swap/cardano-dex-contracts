module ErgoDex.PValidators (
    poolValidator,
    swapValidator,
    depositValidator,
    redeemValidator,
    validatorAddress,
    wrapValidator,
) where

import PlutusLedgerApi.V1.Scripts (Validator (getValidator))
import PlutusLedgerApi.V1.Address

import qualified ErgoDex.PContracts.PDeposit as PD
import qualified ErgoDex.PContracts.PPool    as PP
import qualified ErgoDex.PContracts.PRedeem  as PR
import qualified ErgoDex.PContracts.PSwap    as PS

import Plutarch
import Plutarch.Api.V2 (mkValidator, validatorHash)
import Plutarch.Api.V2.Contexts (PScriptContext)
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)
import Plutarch.Internal
import Ply.Plutarch
import UntypedPlutusCore              (DeBruijn, DefaultFun, DefaultUni, Program)
import PlutusLedgerApi.V1             (Data, ExBudget, CurrencySymbol)
import PlutusLedgerApi.V1.Scripts     (Script (unScript), applyArguments, MintingPolicy, ScriptHash)
import qualified Codec.CBOR.Write as Write
import qualified Data.ByteString.Base16 as Base16

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
poolValidator = mkValidator cfgForValidator $ wrapValidator $ PP.poolValidatorT 5445445974

swapValidator :: Validator
swapValidator = mkValidator cfgForValidator $ wrapValidator $ PS.swapValidatorT 5445445974

depositValidator :: Validator
depositValidator = mkValidator cfgForValidator $ wrapValidator $ PD.depositValidatorT 5445445974

redeemValidator :: Validator
redeemValidator = mkValidator cfgForValidator $ wrapValidator $ PR.redeemValidatorT 5445445974

validatorAddress :: Validator -> Address
validatorAddress = scriptHashAddress . validatorHash
