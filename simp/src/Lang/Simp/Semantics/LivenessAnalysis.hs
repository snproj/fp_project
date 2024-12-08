{-# LANGUAGE ImportQualifiedPost #-}

module Lang.Simp.Semantics.LivenessAnalysis where

import Control.Monad hiding (join)
import Data.Map qualified as DM
import Data.Set qualified as DS
import Lang.Simp.IR.CFG
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.Lattice.CompleteLattice

-- | abstract state is a set of variable names
type AbstractState = DS.Set String

-- | abstract environment is a mapping from label to abstract state
type AbstractEnv = DM.Map Label AbstractState

-- Lab 3 Task 2.1

-- | join(s) = \sqbigcup_{t \in succ(s)} t
join :: [AbstractState] -> AbstractState
join = DS.unions

-- Lab 3 Task 2.1 end

type MonotoneFunction = AbstractEnv -> Either String AbstractEnv

-- | generate the monotone function from a PA program p
genMonotoneFunction :: [LabeledInstr] -> MonotoneFunction
genMonotoneFunction p =
  let cfg = buildCFG p
      top = DS.toList $ DS.fromList (concatMap (allVars . snd) p)
      joinSuccStates :: Label -> AbstractEnv -> AbstractState
      joinSuccStates label env =
        let succs = successors cfg label
            succsStates =
              concatMap
                ( \succ -> case DM.lookup succ env of
                    Nothing -> [DS.empty]
                    Just value -> [value]
                )
                succs
         in join succsStates
      -- Lab 3 Task 2.2
      instrState :: AbstractEnv -> LabeledInstr -> Either String AbstractEnv
      -- \^ case l:t <- src:   s_l = join(s_l) - {t} \cup vars(src)
      -- \^ case l: t <- src1 op src2:  s_l = join(s_l) - {t} \cup vars(src1) \cup vars(src2)
      -- \^ case l: r <- src1 op src2:  s_l = join(s_l) \cup vars(src1) \cup vars(src2)
      -- \^ case l: ifn t goto l':  s_l = join(s_l) \cup {t}
      -- \^ other cases: s_l = join(s_l)
      instrState acc (label, instr) =
        let joinedSuccStates = joinSuccStates label acc
         in case instr of
              -- Case: l : ret, sl = {}
              IRet ->
                Right (DM.insert label DS.empty acc)
              -- Case: l : t ← src, sl = join(sl) − {t} ∪ var(src)
              IMove (Temp (AVar t)) src ->
                let newState = DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src)
                 in Right (DM.insert label newState acc)
              -- Case: l : t ← src1 op src2, sl = join(sl) − {t} ∪ var(src1) ∪ var(src2)
              -- arithmetic ops + less than operator
              IPlus (Temp (AVar t)) src1 src2 ->
                let newState = DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              IMinus (Temp (AVar t)) src1 src2 ->
                let newState = DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              IMult (Temp (AVar t)) src1 src2 ->
                let newState = DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              ILThan (Temp (AVar t)) src1 src2 ->
                let newState = DS.delete t joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              -- Case: l : r ← src, sl = join(sl) ∪ var(src)
              IMove (Regstr r) src ->
                let newState = joinedSuccStates `DS.union` DS.fromList (vars src)
                 in Right (DM.insert label newState acc)
              -- Case: l : r ← src1 op src2, sl = join(sl) ∪ var(src1) ∪ var(src2)
              IPlus (Regstr r) src1 src2 ->
                let newState = joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              IMinus (Regstr r) src1 src2 ->
                let newState = joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              IMult (Regstr r) src1 src2 ->
                let newState = joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              ILThan (Regstr r) src1 src2 ->
                let newState = joinedSuccStates `DS.union` DS.fromList (vars src1 ++ vars src2)
                 in Right (DM.insert label newState acc)
              -- Case: l : ifn t goto l′, sl = join(sl) ∪ {t}
              IIfNot (Temp (AVar t)) _ ->
                let newState = joinedSuccStates `DS.union` DS.singleton t
                 in Right (DM.insert label newState acc)
              -- other instructions, sl = join(sl)
              _ -> Right (DM.insert label joinedSuccStates acc)
   in -- Lab 3 Task 2.2 end
      \absEnv -> foldM instrState absEnv p

-- | Top level function for liveness analysis
--  Peform livness analysis over a PA program `p` return an abstract environment mapping label to abstract states
--   Each abstract state is mapping a set of live variables
analyze :: [LabeledInstr] -> Either String AbstractEnv
analyze p =
  let f = genMonotoneFunction p
      vars = DS.toList $ DS.fromList (concatMap (allVars . snd) p)
      labels = map fst p
      initAbstractState = DS.empty
      initAbstractEnv = DM.fromList (map (\l -> (l, initAbstractState)) labels)
   in naiveFP f initAbstractEnv