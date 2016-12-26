module Analyzer (analyze) where

import Insn

import Data.Array
import Data.List
import Data.Maybe
import Text.Printf (printf)

objArray :: Object -> Array Int Insn
objArray obj = listArray (0, (length obj)) obj

data PipelineState = PipelineState
  { ifetch :: Maybe Int
  , dcd :: Maybe Int
  , mem :: Maybe Int
  , ex :: Maybe Int
  , wb :: Maybe Int
  , staleRegs :: [RegSel] } deriving (Show)

instance Eq PipelineState where
  x == y = (ifetch x) == (ifetch y)
           && (dcd x) == (dcd y)
           && (mem x) == (mem y)
           && (ex x) == (ex y)
           && (wb x) == (wb y)

isValidAddr :: Array Int a -> Int -> Bool
isValidAddr obja i = (b <= i) && (i < t)
  where (b, t) = bounds obja

isValidAddrM :: Array Int a -> Maybe Int -> Bool
isValidAddrM obja (Just i) = isValidAddr obja i
isValidAddrM obja Nothing  = True

isValidState :: Array Int a -> PipelineState -> Bool
isValidState _ PipelineState { ifetch = Nothing
                             , dcd = Nothing
                             , mem = Nothing
                             , ex = Nothing
                             , wb = Nothing } = False
isValidState obja ps = v (ifetch ps) && v (dcd ps) && v (mem ps) && v (ex ps) && v (wb ps)
  where v = isValidAddrM obja

invalidState :: PipelineState
invalidState = PipelineState
               { ifetch = Just 0
               , dcd = Nothing
               , mem = Nothing
               , ex = Nothing
               , wb = Nothing
               , staleRegs = [] }

tryAdvanceAddr :: Array Int a -> Maybe Int -> Maybe Int
tryAdvanceAddr obja (Just i) = if isValidAddr obja (i + 1) then Just (i + 1) else Nothing
tryAdvanceAddr obja Nothing  = Nothing

nextState :: Array Int Insn -> PipelineState -> Maybe Int -> PipelineState
nextState obja curr ifetchAddr = PipelineState
                                 { ifetch = ifetchAddr
                                 , dcd = ifetch curr
                                 , mem = dcd curr
                                 , ex = mem curr
                                 , wb = ex curr
                                 , staleRegs = newStaleRegs}
  where
    staleRegFromAddr addr | isValidAddr obja addr =
      case (obja!addr) of
        ArithInsn{memw=MNone, rd=rd} -> rd
        _                            -> Rc0 
    staleRegFromAddr _ = Rc0

    nonStale = fromMaybe Rc0 $ do addr <- wb curr
                                  Just $ staleRegFromAddr addr

    newStale = fromMaybe Rc0 $ do addr <- ifetchAddr
                                  Just $ staleRegFromAddr addr

    -- delete only deletes the first nonStale, but it is on purpose. There may be multiple stale writes for the same reg in the pipeline.
    filtered = delete nonStale (staleRegs curr) 

    newStaleRegs = if newStale == Rc0 then filtered else newStale:filtered

initialState :: Array Int Insn -> PipelineState 
initialState obja = nextState obja nullState (Just 0)
  where nullState = PipelineState { ifetch = Nothing, dcd = Nothing, mem = Nothing, ex = Nothing, wb = Nothing, staleRegs = [] }

nextStates :: Array Int Insn -> PipelineState -> [PipelineState]
nextStates obja curr = filter (isValidState obja) [bnt, bt]
  where
    bnt :: PipelineState
    bnt = nextState obja curr $ tryAdvanceAddr obja (ifetch curr)

    btm :: Maybe PipelineState
    btm = do jex <- ex curr
             ja <- branchTargetAddr $ obja!jex
             Just $ nextState obja curr (Just ja)
    bt = fromMaybe invalidState btm


possibleStates :: Array Int Insn -> [PipelineState]
possibleStates obja = populateStates obja (initialState obja) []
  where
    populateStates :: Array Int Insn -> PipelineState -> [PipelineState] -> [PipelineState]
    populateStates obja s ss | (elem s ss) = ss -- Already explored
                             | otherwise   = foldr step (s:ss) nexts
                             where 
                               nexts :: [PipelineState]
                               nexts = nextStates obja s

                               step :: PipelineState -> [PipelineState] -> [PipelineState]
                               step = populateStates obja

checkMemRWConflict :: Array Int Insn -> PipelineState -> Maybe String
checkMemRWConflict obja PipelineState{mem=Just mi, wb=Just wbi} =
  case (minsn, wbinsn) of
    (_, ArithInsn{memw=MNone}) -> Nothing
    (ArithInsn{memrs=stgt}, ArithInsn{memw=wtgt}) | stgt == wtgt -> Just (errmsg $ "s = "++(show stgt))
    (ArithInsn{memrt=ttgt}, ArithInsn{memw=wtgt}) | ttgt == wtgt -> Just (errmsg $ "t = "++(show ttgt))
    (_, _) -> Nothing
  where
    minsn  = obja!mi
    wbinsn = obja!wbi
    errmsg rloc = printf (unlines [
      "Found a Memory RW conflict:",
      "  MEM stage reading \"%s\" executing insn:",
      "    %s",
      "  conflicts with WB stage writing to \"%s\" executing insn:",
      "    %s"]) rloc (show minsn) (show $ memw wbinsn) (show wbinsn)

checkMemRWConflict _ _ = Nothing

checkStaleRegRead :: Array Int Insn -> PipelineState -> Maybe String
checkStaleRegRead obja PipelineState{ifetch = Just ifi, staleRegs=staleRegs} | isValidAddr obja ifi
  = if null sregs then Nothing else Just $ errmsg sregs
    where ifinsn = obja!ifi
          rsStale = do rs <- maybeRs ifinsn
                       if elem rs staleRegs then Just rs else Nothing
          rtStale = do rt <- maybeRt ifinsn
                       if elem rt staleRegs then Just rt else Nothing
          sregs = catMaybes [rsStale, rtStale]
          errmsg :: [RegSel] -> String
          errmsg sregs = printf (unlines [
            "Found a stale reg read of registers %s:",
            "  %s"]) (show sregs) (show ifinsn)
checkStaleRegRead _ _ = Nothing
  
type CheckFunc = Array Int Insn -> PipelineState -> Maybe String

analyze :: Object -> [String]
analyze obj = pipelineCheckResults
  where
    obja :: Array Int Insn
    obja = objArray obj

    states :: [PipelineState]
    states = possibleStates obja

    checks :: [CheckFunc]
    checks = [checkStaleRegRead, checkMemRWConflict]

    pipelineCheckResults :: [String]
    pipelineCheckResults = checks >>= (\check -> catMaybes $ map (check obja) states)
