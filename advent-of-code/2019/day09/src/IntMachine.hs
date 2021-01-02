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
import Data.Int (Int64)

data HaltedState = Continue | Halted deriving (Show)

type Memory = Map Int64 Int64

newtype PC = PC Int64 deriving (Show)

newtype Input = Input [Int64] deriving (Show)

newtype RevOutput = RevOutput [Int64] deriving (Show)

newtype RelBase = RelBase Int64 deriving (Show)

data State
  = State
      Memory
      !HaltedState
      !PC
      Input
      RevOutput
      RelBase
  deriving (Show)

data FinalState = FinalState [Int64] [Int64] deriving (Show)

data ParamType = PAddr | PImm | PRel deriving (Show)

data Param = Param ParamType !Int64 deriving (Show)

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
  | OpAdjRelBase
  deriving (Show)

newtype ParamCode = ParamCode Int64 deriving (Show)

intToOpcode :: Int64 -> OpCode
intToOpcode 1 = OpAdd
intToOpcode 2 = OpMul
intToOpcode 3 = OpIn
intToOpcode 4 = OpOut
intToOpcode 5 = OpJmpTrue
intToOpcode 6 = OpJmpFalse
intToOpcode 7 = OpLT
intToOpcode 8 = OpEQ
intToOpcode 9 = OpAdjRelBase
intToOpcode 99 = OpHalt
intToOpcode _ = undefined

paramGet :: State -> Param -> Int64
paramGet (State mem _ _ _ _ _) (Param PAddr p) = Map.findWithDefault 0 p mem
paramGet _ (Param PImm v) = v
paramGet (State mem _ _ _ _ (RelBase rb)) (Param PRel p) = Map.findWithDefault 0 (rb + p) mem

paramSet :: State -> Param -> Int64 -> Memory
paramSet (State mem _ _ _ _ _) (Param PAddr p) v = Map.insert p v mem
paramSet _ (Param PImm _) _ = undefined
paramSet (State mem _ _ _ _ (RelBase rb)) (Param PRel p) v = Map.insert (rb + p) v mem

pcAdd :: PC -> Int64 -> PC
(PC pc) `pcAdd` offset = PC (pc + offset)

execArithOp :: (Int64 -> Int64 -> Int64) -> Param -> Param -> Param -> State -> State
execArithOp opFn left right dest state@(State _ hState pc in_ out rb) =
  State (paramSet state dest result) hState (pc `pcAdd` 4) in_ out rb
  where
    result = paramGet state left `opFn` paramGet state right

execIn :: Param -> State -> State
execIn dest state@(State _ hState pc in_ out rb) =
  State (paramSet state dest inVal) hState (pc `pcAdd` 2) (Input inRest) out rb
  where
    (inVal, inRest) = case in_ of
      Input (a : b) -> (a, b)
      Input [] -> error "reached end of input while reading"

execOut :: Param -> State -> State
execOut p state@(State mem hState pc in_ (RevOutput revOut) rb) =
  State mem hState (pc `pcAdd` 2) in_ newOut rb
  where
    newOut = RevOutput $ paramGet state p : revOut

execJumpParamCond :: (Int64 -> Bool) -> Param -> Param -> State -> State
execJumpParamCond test testParam jumpTo state@(State mem hState pc in_ out rb) =
  State mem hState newPc in_ out rb
  where
    newPc =
      if test (paramGet state testParam)
        then PC $ paramGet state jumpTo
        else pc `pcAdd` 3

execCmp :: (Int64 -> Int64 -> Bool) -> Param -> Param -> Param -> State -> State
execCmp cmpFn left right dest state@(State _ hState pc in_ out rb) =
  State (paramSet state dest result) hState (pc `pcAdd` 4) in_ out rb
  where
    result = if paramGet state left `cmpFn` paramGet state right then 1 else 0

execAdjRelBase :: Param -> State -> State
execAdjRelBase param state@(State mem hState pc in_ out (RelBase rb)) =
  State mem hState (pc `pcAdd` 2) in_ out (RelBase (rb + adjust))
  where
    adjust = paramGet state param

execHalt :: State -> State
execHalt (State mem _ pc in_ out rb) = State mem Halted pc in_ out rb

mapParams :: ParamCode -> [Int64] -> [Param]
mapParams _ [] = []
mapParams (ParamCode code) (x : xs) =
  paramOf (code `mod` 10) x : mapParams (ParamCode (code `div` 10)) xs
  where
    paramOf 0 p = Param PAddr p
    paramOf 1 v = Param PImm v
    paramOf 2 v = Param PRel v
    paramOf cd _ = error (printf "unexpected param type code %d decoding code %s" cd (show code))

withParams0 :: (State -> State) -> ParamCode -> [Int64] -> State -> State
withParams0 fn _ _ = fn

withParams1 :: (Param -> State -> State) -> ParamCode -> [Int64] -> State -> State
withParams1 fn paramCode vals = case mapParams paramCode vals of
  a : _ -> fn a
  _ -> undefined

withParams2 :: (Param -> Param -> State -> State) -> ParamCode -> [Int64] -> State -> State
withParams2 fn paramCode vals = case mapParams paramCode vals of
  a : b : _ -> fn a b
  _ -> undefined

withParams3 :: (Param -> Param -> Param -> State -> State) -> ParamCode -> [Int64] -> State -> State
withParams3 fn paramCode vals = case mapParams paramCode vals of
  a : b : c : _ -> fn a b c
  _ -> undefined

execOp :: OpCode -> ParamCode -> [Int64] -> State -> State
execOp OpAdd = withParams3 $ execArithOp (+)
execOp OpMul = withParams3 $ execArithOp (*)
execOp OpIn = withParams1 execIn
execOp OpOut = withParams1 execOut
execOp OpJmpTrue = withParams2 $ execJumpParamCond (/= 0)
execOp OpJmpFalse = withParams2 $ execJumpParamCond (== 0)
execOp OpLT = withParams3 $ execCmp (<)
execOp OpEQ = withParams3 $ execCmp (==)
execOp OpAdjRelBase = withParams1 execAdjRelBase
execOp OpHalt = withParams0 execHalt

execOpEncoded :: [Int64] -> State -> State
execOpEncoded [] = undefined
execOpEncoded (opVal : rest) = execOp opCode paramCode rest
  where
    opCode = intToOpcode $ opVal `mod` 100
    paramCode = ParamCode $ opVal `div` 100

memListAt :: Int64 -> Memory -> [Int64]
memListAt pc memory = memory Map.! pc : memListAt (pc + 1) memory

step :: State -> State
step (State _ Halted _ _ _ _) = undefined
step state@(State memory Continue (PC pc) _ _ _) = execOpEncoded (memListAt pc memory) state

simulate :: State -> [State]
simulate state@(State _ Halted _ _ _ _) = [state]
simulate state = state : simulate (step state)

simulateOutput :: State -> [Int64]
simulateOutput (State _ Halted _ _ _ _) = []
simulateOutput state@(State _ _ _ _ (RevOutput []) _) = simulateOutput $ step state
simulateOutput (State mem hState pc in_ (RevOutput (x : xs)) rb) =
  x : simulateOutput (State mem hState pc in_ (RevOutput xs) rb)

simulateFinal :: State -> FinalState
simulateFinal state = FinalState (memToList memory) (reverse revOutput)
  where
    State memory _ _ _ (RevOutput revOutput) _ = last $ simulate state

readMemory :: T.Text -> Memory
readMemory input =
  Map.fromList $ zip [0 ..] $ map (read . T.unpack) $ T.splitOn (T.pack ",") $ T.strip input

makeState :: Memory -> State
makeState = makeStateWithInput []

makeStateWithInput :: [Int64] -> Memory -> State
makeStateWithInput input memory =
  State memory Continue (PC 0) (Input input) (RevOutput []) (RelBase 0)

patchState :: [(Int64, Int64)] -> State -> State
patchState lst (State mem halted pc input output rb) =
  State (Map.union (Map.fromList lst) mem) halted pc input output rb

memToList :: Memory -> [Int64]
memToList mem = go lower
  where
    lower = fst $ Map.findMin mem
    upper = fst $ Map.findMax mem
    go i | lower <= i && i <= upper = fromMaybe (minBound :: Int64) (Map.lookup i mem) : go (i + 1)
    go _ = []
