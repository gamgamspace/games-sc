{-# STDLIB_VERSION 6 #-}
{-# CONTENT_TYPE DAPP #-}
{-# SCRIPT_TYPE ACCOUNT #-}

# Till 2039537 height:
# GAME RECORD FORMAT:
#   gameId -> nnState_nnPlayerChoice_nnPlayerPublicKey_nnBetHeight_nnPotentialWinAmount_nnRandom_nnTIMEOUT
# where nn - defining number of characters for particular property
# Example:
#   123 -> 03WON_0512345_09taaWFeFzV_06575247_09190000000_014_07TIMEOUT
#
# After 2039537 height:
# GAME RECORD FORMAT:
#   gameId -> State_PlayerChoice_PlayerPublicKey_BetHeight_PotentialWinAmount_BetAssetId_Random_TIMEOUT
#
#   Example:
#   123 -> WON_12345_taaWFeFzV_575247_190000000_1_4_TIMEOUT
#
#   +-------------------------------+--------------+--------------------------------------+
#   | property description          | value        | possible values and notes            |
#   +-------------------------------+--------------+--------------------------------------+
#   | game id                       | 123          |                                      |
#   | game state                    | WON          | SUBMITTED | WON | LOST               |
#   | player's choice               | 12345        |                                      |
#   | player's public key           | taaWFeFzV    |                                      |
#   | bet's height                  | 575247       |                                      |
#   | potential win amount          | 190000000    |                                      |
#   | bet asset id                  | 1            | 0 for WAVES, 1 for USDN              |
#   | generated random number       | 4            | exists only for WON and LOST states  |
#   | was game finished by timeout? | TIMEOUT      | optional attribute                   |
#   +-------------------------------+--------------+--------------------------------------+

# CONSTANTS declaration
let SEP = "__"
let WAVESID = base58'WAVES'
let WAVESD      = 100000000

func getStrOrFail(address: Address, key: String) = address.getString(key).valueOrErrorMessage(makeString(["mandatory ", address.toString(), ".", key, " is not defined"], ""))
func getIntOrFail(address: Address, key: String) = address.getInteger(key).valueOrErrorMessage(makeString(["mandatory ", address.toString(), ".", key, " is not defined"], ""))
func getBoolOrFail(address: Address, key: String) = address.getBoolean(key).valueOrErrorMessage(makeString(["mandatory ", address.toString(), ".", key, " is not defined"], ""))

let allowedAssetsKey    = "%s%s__cfg__allowedAssets"
let assetsDecimalsKey   = "%s%s__cfg__assetsDecimals"
let betDividersKey      = "%s%s__cfg__assetsBetDividers"
let RSAPUBLIC64KEY      = "%s%s__cfg__rsaPublic64"
let SERVERADDRESSKEY    = "%s%s__cfg__benzAddress"
let RANDTIMEFRAMEKEY    = "%s%s__cfg__withdrawTimeFrame"
let GAMESCOUNTERKEY     = "%s%s__runtime__gameNum"
let blockedKey          = "%s%s__runtime__contractIsBlocked"
let reservedAmountsKey  = "%s%s__runtime__reservedAmounts"

func getIntArray(key: String) = {
    let a = this.getStrOrFail(key)
    func filler(acc: List[Int], el: String) = {
        acc :+ el.parseIntValue()
    }
    FOLD<10>(a.split(SEP), [], filler)
}

func getAssets() = {
  this.getStrOrFail(allowedAssetsKey).split(SEP)
}

let ASSETS = getAssets()
let DECIMALS = getIntArray(assetsDecimalsKey)
let BETDIVIDERS = getIntArray(betDividersKey)
func RESERVATIONKEYBYSTR(assetStr: String) = "$RESERVED_AMOUNT_" + assetStr
func RESERVATIONKEY(assetIdx: Int) = RESERVATIONKEYBYSTR(ASSETS[assetIdx])

let MINFEEWAVES = 5 * WAVESD / 1000 #0.005

let idxAssets      = 0
let idxDecimals    = 1
let idxDividers    = 2
let idxReservation = 3

func getBetMin(assetId: Int) =  1 * DECIMALS[assetId] / 2   # 0.5 Waves/usd
func getBetMax(assetId: Int) = 6 * DECIMALS[assetId]        # 6.0 Waves/usd
func getBetStep(assetId: Int) = 1 * DECIMALS[assetId] / 10  # 0.1 Waves/usd

let PRECISION = 10000

# RANGES (DIAPASONS)
let R1MAX = 96
let R1MIN = 94
let R1K   = 9860 # 0.9860 x PRECISION or (100-1.4)/100 x PRECISION

let R2MAX = 93
let R2MIN = 87
let R2K   = 9800 # 0.9800 x PRECISION or (100-2.0)/100 x PRECISION

let R3MAX = 86
let R3MIN = 86
let R3K   = 9750 # 0.9750 x PRECISION or (100-2.5)/100 x PRECISION

let R4MAX = 85
let R4MIN = 84
let R4K   = 9670 # 0.9670 x PRECISION or (100-3.3)/100 x PRECISION

let R5MAX = 83
let R5MIN = 83
let R5K   = 9630 # 0.9630 x PRECISION or (100-3.7)/100 x PRECISION

let R6MAX = 82
let R6MIN = 67
let R6K   = 9610 # 0.9610 x PRECISION or (100-3.9)/100 x PRECISION

let R7MAX = 66
let R7MIN = 56
let R7K   = 9560 # 0.9560 x PRECISION or (100-4.4)/100 x PRECISION

let R8MAX = 55
let R8MIN = 38
let R8K   = 9500 # 0.9500 x PRECISION or (100-5)/100 x PRECISION

let R9MAX = 37
let R9MIN = 3
let R9K   = 9290 # 0.9290 x PRECISION or (100-7.1)/100 x PRECISION

let R10MAX = 2
let R10MIN = 1
let R10K   = 9860 # 0.9860 x PRECISION or (100-1.4)/100 x PRECISION

let IdxGameState        = 0
let IdxPlayerChoice     = 1
let IdxPlayerPubKey58   = 2
let IdxStartedHeight    = 3
let IdxWinAmount        = 4
let IdxAssetId          = 5
#let IdxRandOrEmpty     = 6

let STATESUBMITTED  = "SUBMITTED"
let STATEWON        = "WON"
let STATELOST       = "LOST"

func getStringOrFail(key: String) = this.getString(key).valueOrErrorMessage(key + " key is not specified in this.state")

# TODO consider to create single config
let RSAPUBLIC = getStringOrFail(RSAPUBLIC64KEY).fromBase64String()
let SERVER = getStringOrFail(SERVERADDRESSKEY).addressFromStringValue()
# timeframe to initiate withdraw by oracle otherwise player automatically will be a winner, 7200 by default
let RANDORACLETIMEFRAME = this.getInteger(RANDTIMEFRAMEKEY).valueOrElse(7200)

func getIntOr(key: String, default: Int) = if isDefined(getInteger(key)) then getIntegerValue(key) else default
func setInt(key: String, value: Int) = IntegerEntry(key, value)
func incrementInt(key: String) = key.setInt(key.getIntOr(-1) + 1)
func changeInt(key: String, by: Int) = key.setInt(key.getIntOr(0) + by)

func assetIdToStr(assetIdOrUnit: ByteVector|Unit) = {
  match assetIdOrUnit {
    case b: ByteVector => b.toBase58String()
    case _ => "WAVES"
  }
}

func assetIdFromStr(assetIdStr: String) = {
    if (assetIdStr == "WAVES") then unit else assetIdStr.fromBase58String()
}

func getAssetBalance(assetIdOrUnit: ByteVector|Unit) = {
  match assetIdOrUnit {
    case assetId: ByteVector => this.assetBalance(assetId)
    case _ => this.wavesBalance().available
  }
}

func increaseReserveAmount(winAmount: Int, assetIdx: Int) = {
    let assetIdStr = ASSETS[assetIdx]
    let newReservedAmount = RESERVATIONKEY(assetIdx).getIntOr(0) + winAmount
    if newReservedAmount > assetIdStr.assetIdFromStr().getAssetBalance() then throw("Insufficient funds on Dice Roller account. Transaction was rejected for your safety.") else
    newReservedAmount
}

func decreaseReservedAmount(gameId: String, assetIdx: Int, winAmount: Int) = {
    if RESERVATIONKEY(assetIdx).getIntOr(0) - winAmount < 0 then throw("Invalid Dice Roller account state - reserved amount is less than 0") else
    RESERVATIONKEY(assetIdx).changeInt(-winAmount)
}

func validateAndGetAssetIdx(assetIdStr: String) = {
    let idx = ASSETS.indexOf(assetIdStr)
    if (!idx.isDefined()) then throw("Invalid payment asset") else idx.value()
}

func validateBetAndGetWinAmount(bet: Int, internalAssetIdx: Int, playerChoice: String) = {
    let BETMIN = getBetMin(internalAssetIdx)
    let BETMAX = getBetMax(internalAssetIdx)
    let BETSTEP = getBetStep(internalAssetIdx)
    let playerChoiceInt = parseIntValue(playerChoice)
    let betAmountValid = bet >= BETMIN && bet <= BETMAX && bet % BETSTEP == 0
    let playerChoiceValid = playerChoiceInt >= 1 && playerChoiceInt <= 96

    if (!betAmountValid) then throw("Bet amount is not in range: minBet=" + toString(BETMIN)
                                                + " maxBet=" + toString(BETMAX)
                                                + " betStep=" + toString(BETSTEP)) else
    if (!playerChoiceValid) then throw("Player choice is out of the condition below: 1 <= choice <= 96") else

    let RKxPrecision =
        if (playerChoiceInt <= R1MAX && playerChoiceInt >= R1MIN) then { R1K } else
        if (playerChoiceInt <= R2MAX && playerChoiceInt >= R2MIN) then { R2K } else
        if (playerChoiceInt <= R3MAX && playerChoiceInt >= R3MIN) then { R3K } else
        if (playerChoiceInt <= R4MAX && playerChoiceInt >= R4MIN) then { R4K } else
        if (playerChoiceInt <= R5MAX && playerChoiceInt >= R5MIN) then { R5K } else
        if (playerChoiceInt <= R6MAX && playerChoiceInt >= R6MIN) then { R6K } else
        if (playerChoiceInt <= R7MAX && playerChoiceInt >= R7MIN) then { R7K } else
        if (playerChoiceInt <= R8MAX && playerChoiceInt >= R8MIN) then { R8K } else
        if (playerChoiceInt <= R9MAX && playerChoiceInt >= R9MIN) then { R9K } else
        if (playerChoiceInt <= R10MAX && playerChoiceInt >= R10MIN) then { R10K} else {throw("Couldn't define range: playerChoice=" + playerChoice)}

    (((100 * RKxPrecision) / playerChoiceInt) * bet) / PRECISION
}

func randToStr(r: Int) = {
    if (r >= 1 && r <= 100) then {
        toString(r)
    } else {
        throw("Unsupported r parameter passed: expected=[1,...,100] actual=" + toString(r))
    }
}

# @return 1 ... 100
func generateRandChoise(gameId: String, rsaSign: ByteVector) = {
    # verify RSA signature to proof random
    let rsaSigValid = rsaVerify_16Kb(SHA256, toBytes(gameId), rsaSign, RSAPUBLIC)
    if !rsaSigValid then throw("Invalid RSA signature") else

    let rand = toInt(sha256(rsaSign)) % 100
    let moduleRand = if rand < 0 then -1 * rand else rand
    let finalRand = moduleRand + 1
    let finalRandStr = finalRand.toString()

    if (finalRand < 1 || finalRand > 100) then throw("Unsupported r parameter passed: expected=[1,...,100] actual=" + finalRandStr) else
    finalRandStr
}

func isPlayerWin(playerChoice: String, randChoice: String) = {
    let playerChoiceInt = parseIntValue(playerChoice)
    let randChoiceInt = parseIntValue(randChoice)
    playerChoiceInt >= randChoiceInt
}

func formatGameDataS(gameStatus: String, playerChoice: String, playerPubKey58: String,
                    startedHeight: String, winAmount: String, assetIdx: String, randOrEmpty: String) = {
    makeString([
        gameStatus,
        playerChoice,
        playerPubKey58,
        startedHeight,
        winAmount,
        assetIdx,
        (if randOrEmpty == "" then "" else randOrEmpty)],
    "_")
}

func formatGameData(gameStatus: String, playerChoice: String, playerPubKey58: String,
                    startedHeight: Int, winAmount: Int, assetIdx: Int, randOrEmpty: String) = {
  formatGameDataS(gameStatus,
      playerChoice,
      playerPubKey58,
      startedHeight.toString(),
      winAmount.toString(),
      assetIdx.toString(),
      randOrEmpty)
}

func finishGameData(origGameData: List[String], gameStatus: String, rand: String, winByTimeout: Boolean) = {
  let finishGameData = formatGameDataS(gameStatus,
      origGameData[IdxPlayerChoice],
      origGameData[IdxPlayerPubKey58],
      origGameData[IdxStartedHeight],
      origGameData[IdxWinAmount],
      origGameData[IdxAssetId],
      rand)
  if (winByTimeout) then finishGameData + "_TIMEOUT" else finishGameData
}

# @returns LIST[gameState, playerChoice, playerPublicKey, startedHeight, winAmount, assetId]
func extractGameData(gameId: String) = {
    (match getString(this, gameId) {
        case str:String => str
        case _ => throw("Game: " + gameId + " not found.")
    }).split("_")
}

@Callable(i)
func maintenance(blocked: Boolean) = {
    if (i.caller != SERVER) then throw("not authorized") else
    [BooleanEntry(blockedKey, blocked)]
}

@Callable(i)
func bet(playerChoice: String) = {
    if (getBoolean(blockedKey).valueOrElse(false)) then throw("Game is stopped for maintenence") else

    let gameId = toBase58String(i.transactionId)

    if (i.payments.size() <= 1) then throw("2 payments must be attached") else
    if (isDefined(getString(this, gameId))) then throw("Bet for: " + gameId + " was already made.") else

    let betPmt = i.payments[0].value()
    let feePmt = i.payments[1].value()
    if (feePmt.assetId.isDefined())  then throw("feePmt (2nd payment) assetId must be in Waves") else
    if (feePmt.amount < MINFEEWAVES) then throw("feePmt (2nd payment) must be >= 0.005 Waves") else

    let assetIdStr = assetIdToStr(betPmt.assetId)
    let internalAssetIdx = validateAndGetAssetIdx(assetIdStr)
    let commission = feePmt.amount

    let winAmount = validateBetAndGetWinAmount(betPmt.amount, internalAssetIdx, playerChoice)

    let playerPubKey58 = toBase58String(i.callerPublicKey)
    let gameData = formatGameData(
            STATESUBMITTED, playerChoice, playerPubKey58, height, winAmount, internalAssetIdx, "")

    [IntegerEntry(RESERVATIONKEY(internalAssetIdx), increaseReserveAmount(winAmount, internalAssetIdx)),
      GAMESCOUNTERKEY.incrementInt(),
      StringEntry(gameId, gameData),
      ScriptTransfer(SERVER, commission, feePmt.assetId)]
}

@Callable(i)
func withdraw(gameId: String, rsaSign: ByteVector) = {
    # verify that gameId is an invocation transaction
    # verify that gameId has not been used before
    # verify that betTx.recipient == this

    # verify height of gameId if currentHeight - betTxHeight > 2880 then winner is player

    # this condition verifies gameId and as a result there is no need to check recipient and sender
    # everything has been checked during bet stage
    let gameData = extractGameData(gameId)

    let gameState       = gameData[IdxGameState]
    let playerChoice    = gameData[IdxPlayerChoice]
    let startedHeight   = gameData[IdxStartedHeight].parseIntValue()
    let winAmount       = gameData[IdxWinAmount].parseIntValue()
    let assetIdx         = gameData[IdxAssetId].parseIntValue()
    let playerPubKey58  = gameData[IdxPlayerPubKey58]
    let playerAddress   = addressFromPublicKey(fromBase58String(playerPubKey58))

    if (gameState != STATESUBMITTED) then throw("Invalid game state for passed gameId") else
    if (i.caller != SERVER) then throw("Regular withdraw can be done by server only") else

    # if oracle doesn't initiate withdraw in specified timeframe then player will be a winner
    let winByTimeout = height - startedHeight > RANDORACLETIMEFRAME
    let randChoise = if (winByTimeout) then playerChoice else generateRandChoise(gameId, rsaSign)

    let playerWin     = isPlayerWin(playerChoice, randChoise)
    let newGameStatus = if (playerWin) then STATEWON else STATELOST
    let newGameData   = finishGameData(gameData, newGameStatus, randChoise, winByTimeout)

    [StringEntry(gameId, newGameData),
      decreaseReservedAmount(gameId, assetIdx, winAmount)]
    ++
    if (playerWin) then [ScriptTransfer(playerAddress, winAmount, assetIdFromStr(ASSETS[assetIdx]))] else []
}

@Verifier(tx)
func verify() = {
    sigVerify(tx.bodyBytes, tx.proofs[0], tx.senderPublicKey) &&
    match tx {
        case ttx:TransferTransaction => {
            let assetIdx = validateAndGetAssetIdx(assetIdToStr(ttx.assetId))
            # do not allow to transfer reserved funds
            getAssetBalance(ttx.assetId) - ttx.amount >= RESERVATIONKEY(assetIdx).getIntOr(0)
        }
        case stx:SetScriptTransaction => {
            # allow to modify script only in case if all reserved amounts are 0
            func checker(acc: Boolean, el: String) = acc && (RESERVATIONKEYBYSTR(el).getIntOr(0) == 0)

            FOLD<10>(ASSETS, true, checker)
        }
        case _ => false
    }
}
