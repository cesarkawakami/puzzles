module IntMachine
  ( FinalState (..),
    simulate,
    simulateOutput,
    simulateFinal,
    readMemory,
    makeState,
    makeStateWithInput,
    patchState,
    memToList,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Text.Printf (printf)

data HaltedState = Continue | Halted deriving (Show)

type Memory = Map Int Int

newtype PC = PC Int deriving (Show)

newtype Input = Input [Int] deriving (Show)

newtype RevOutput = RevOutput [Int] deriving (Show)

data State = State Memory !HaltedState !PC Input RevOutput deriving (Show)

data FinalState = FinalState [Int] [Int] deriving (Show)

data ParamType = PAddr | PImm deriving (Show)

data Param = Param ParamType !Int deriving (Show)

data OpCode
  = OpAdd
  | OpMul
  | OpIn
  | OpOut
  | OpHalt
  | OpJmpTrue
  | OpJmpFalse
  | OpLT
  | OpEQ
  deriving (Show)

newtype ParamCode = ParamCode Int deriving (Show)

intToOpcode :: Int -> OpCode
intToOpcode 1 = OpAdd
intToOpcode 2 = OpMul
intToOpcode 3 = OpIn
intToOpcode 4 = OpOut
intToOpcode 5 = OpJmpTrue
intToOpcode 6 = OpJmpFalse
intToOpcode 7 = OpLT
intToOpcode 8 = OpEQ
intToOpcode 99 = OpHalt
intToOpcode _ = undefined

paramGet :: Memory -> Param -> Int
paramGet mem (Param PAddr p) = mem Map.! p
paramGet _ (Param PImm v) = v

paramSet :: Memory -> Param -> Int -> Memory
paramSet mem (Param PAddr p) v = Map.insert p v mem
paramSet _ (Param PImm _) _ = undefined

pcAdd :: PC -> Int -> PC
(PC pc) `pcAdd` offset = PC (pc + offset)

execArithOp :: (Int -> Int -> Int) -> Param -> Param -> Param -> State -> State
execArithOp opFn left right dest (State mem hState pc in_ out) =
  State (paramSet mem dest result) hState (pc `pcAdd` 4) in_ out
  where
    result = paramGet mem left `opFn` paramGet mem right

execIn :: Param -> State -> State
execIn dest (State mem hState pc in_ out) =
  State (paramSet mem dest inVal) hState (pc `pcAdd` 2) (Input inRest) out
  where
    (inVal, inRest) = case in_ of
      Input (a : b) -> (a, b)
      Input [] -> error "reached end of input while reading"

execOut :: Param -> State -> State
execOut p (State mem hState pc in_ (RevOutput revOut)) =
  State mem hState (pc `pcAdd` 2) in_ newOut
  where
    newOut = RevOutput $ paramGet mem p : revOut

execJumpParamCond :: (Int -> Bool) -> Param -> Param -> State -> State
execJumpParamCond test testParam jumpTo (State mem hState pc in_ out) =
  State mem hState newPc in_ out
  where
    newPc =
      if test (paramGet mem testParam)
        then PC $ paramGet mem jumpTo
        else pc `pcAdd` 3

execCmp :: (Int -> Int -> Bool) -> Param -> Param -> Param -> State -> State
execCmp cmpFn left right dest (State mem hState pc in_ out) =
  State (paramSet mem dest result) hState (pc `pcAdd` 4) in_ out
  where
    result = if paramGet mem left `cmpFn` paramGet mem right then 1 else 0

execHalt :: State -> State
execHalt (State mem _ pc in_ out) = State mem Halted pc in_ out

mapParams :: ParamCode -> [Int] -> [Param]
mapParams _ [] = []
mapParams (ParamCode code) (x : xs) =
  paramOf (code `mod` 10) x : mapParams (ParamCode (code `div` 10)) xs
  where
    paramOf 0 p = Param PAddr p
    paramOf 1 v = Param PImm v
    paramOf cd _ = error (printf "unexpected param type code %d decoding code %s" cd (show code))

withParams0 :: (State -> State) -> ParamCode -> [Int] -> State -> State
withParams0 fn _ _ = fn

withParams1 :: (Param -> State -> State) -> ParamCode -> [Int] -> State -> State
withParams1 fn paramCode vals = case mapParams paramCode vals of
  a : _ -> fn a
  _ -> undefined

withParams2 :: (Param -> Param -> State -> State) -> ParamCode -> [Int] -> State -> State
withParams2 fn paramCode vals = case mapParams paramCode vals of
  a : b : _ -> fn a b
  _ -> undefined

withParams3 :: (Param -> Param -> Param -> State -> State) -> ParamCode -> [Int] -> State -> State
withParams3 fn paramCode vals = case mapParams paramCode vals of
  a : b : c : _ -> fn a b c
  _ -> undefined

execOp :: OpCode -> ParamCode -> [Int] -> State -> State
execOp OpAdd = withParams3 $ execArithOp (+)
execOp OpMul = withParams3 $ execArithOp (*)
execOp OpIn = withParams1 execIn
execOp OpOut = withParams1 execOut
execOp OpJmpTrue = withParams2 $ execJumpParamCond (/= 0)
execOp OpJmpFalse = withParams2 $ execJumpParamCond (== 0)
execOp OpLT = withParams3 $ execCmp (<)
execOp OpEQ = withParams3 $ execCmp (==)
execOp OpHalt = withParams0 execHalt

execOpEncoded :: [Int] -> State -> State
execOpEncoded [] = undefined
execOpEncoded (opVal : rest) = execOp opCode paramCode rest
  where
    opCode = intToOpcode $ opVal `mod` 100
    paramCode = ParamCode $ opVal `div` 100

memListAt :: Int -> Memory -> [Int]
memListAt pc memory = memory Map.! pc : memListAt (pc + 1) memory

step :: State -> State
step (State _ Halted _ _ _) = undefined
step state@(State memory Continue (PC pc) _ _) = execOpEncoded (memListAt pc memory) state

simulate :: State -> [State]
simulate state@(State _ Halted _ _ _) = [state]
simulate state = state : simulate (step state)

simulateOutput :: State -> [Int]
simulateOutput (State _ Halted _ _ _) = []
simulateOutput state@(State _ _ _ _ (RevOutput [])) = simulateOutput $ step state
simulateOutput (State mem hState pc in_ (RevOutput (x : xs))) =
  x : simulateOutput (State mem hState pc in_ (RevOutput xs))

simulateFinal :: State -> FinalState
simulateFinal state = FinalState (memToList memory) (reverse revOutput)
  where
    State memory _ _ _ (RevOutput revOutput) = last $ simulate state

readMemory :: T.Text -> Memory
readMemory input =
  Map.fromList $ zip [0 ..] $ map (read . T.unpack) $ T.splitOn (T.pack ",") $ T.strip input

makeState :: Memory -> State
makeState memory = State memory Continue (PC 0) (Input []) (RevOutput [])

makeStateWithInput :: [Int] -> Memory -> State
makeStateWithInput input memory = State memory Continue (PC 0) (Input input) (RevOutput [])

patchState :: [(Int, Int)] -> State -> State
patchState lst (State mem halted pc input output) =
  State (Map.union (Map.fromList lst) mem) halted pc input output

memToList :: Memory -> [Int]
memToList mem = go lower
  where
    lower = fst $ Map.findMin mem
    upper = fst $ Map.findMax mem
    go i | lower <= i && i <= upper = fromMaybe (minBound :: Int) (Map.lookup i mem) : go (i + 1)
    go _ = []
