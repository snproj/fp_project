module Lang.Simp.IR.MMUpDown where

import Control.Monad.State
import Lang.Simp.IR.PseudoAssembly
import Lang.Simp.IR.Util (StateInfo, chkNextLabel, newLabel, newTemp, var2AVar)
import Lang.Simp.Syntax.AST

type UpE = Opr

type DownE = [Instr]

cogenExp :: Exp -> State StateInfo (Opr, [LabeledInstr])
-- GE(c) |- (c, [])      (Const)
cogenExp (ConstExp (IntConst v)) = return (IntLit v, [])
-- true into 1, false into 0
cogenExp (ConstExp (BoolConst True)) = return (IntLit 1, [])
cogenExp (ConstExp (BoolConst False)) = return (IntLit 0, [])
-- GE(X) |- (X, [])      (Var)
cogenExp (VarExp v) =
  let av = var2AVar v
   in return (Temp av, [])
{-
GE(e) |- (up_e, down_e)
------------------------- (Paren)
GE((e)) |- (up_e, down_e)
-}
cogenExp (ParenExp e) = cogenExp e
-- Lab 1 Task 2.1
{-
GE(e1) |- (up_e1, down_e1)
GE(e2) |- (up_e2, down_e2)
X is a fresh var
L is a fresh label
--------------------------------------------------------------------- (Op)
GE(e1 op e2) |- (X, down_e1 ++ down_e2 ++ [L:X <- up_e1 op up_e2])
-}
cogenExp (Plus e1 e2) = do
  (op1, instrs1) <- cogenExp e1
  (op2, instrs2) <- cogenExp e2
  result <- newTemp
  lbl <- newLabel
  let instrs = instrs1 ++ instrs2 ++ [(lbl, IPlus result op1 op2)]
  return (result, instrs)
cogenExp (Minus e1 e2) = do
  (op1, instrs1) <- cogenExp e1
  (op2, instrs2) <- cogenExp e2
  result <- newTemp
  lbl <- newLabel
  let instrs = instrs1 ++ instrs2 ++ [(lbl, IMinus result op1 op2)]
  return (result, instrs)
cogenExp (Mult e1 e2) = do
  (op1, instrs1) <- cogenExp e1
  (op2, instrs2) <- cogenExp e2
  result <- newTemp
  lbl <- newLabel
  let instrs = instrs1 ++ instrs2 ++ [(lbl, IMult result op1 op2)]
  return (result, instrs)
cogenExp (DEqual e1 e2) = do
  (op1, instrs1) <- cogenExp e1
  (op2, instrs2) <- cogenExp e2
  result <- newTemp
  lbl <- newLabel
  let instrs = instrs1 ++ instrs2 ++ [(lbl, IDEqual result op1 op2)]
  return (result, instrs)
cogenExp (LThan e1 e2) = do
  (op1, instrs1) <- cogenExp e1
  (op2, instrs2) <- cogenExp e2
  result <- newTemp
  lbl <- newLabel
  let instrs = instrs1 ++ instrs2 ++ [(lbl, ILThan result op1 op2)]
  return (result, instrs)

-- Lab 1 Task 2.1 end

class Cogen t where
  cogen :: t -> State StateInfo [LabeledInstr]

{-
    for i in {1,n}    G(stmt_i) |- instrs_i
-------------------------------------------------------- (Sequence)
    G(stmt_1,...,stmt_n) |- instrs_1 ++ ... ++  instrs_n
-}

instance (Cogen t) => Cogen [t] where
  cogen ts = do
    x <- mapM cogen ts
    return (concat x)

instance Cogen Stmt where
  cogen stmt = case stmt of
    {-
            ------------- (Nop)
            G(nop) |- []
    -}
    Nop -> return []
    {-
            GE(e) |- (up_e, down_e)
            L is a new label
            -------------------------------------- (Assign)
            G(X = e) |- down_e ++ [ L: X <- up_e]
    -}
    Assign v e -> do
      (u, d) <- cogenExp e
      lbl <- newLabel
      let av = var2AVar v
          i = IMove (Temp av) u
      return (d ++ [(lbl, i)])
    {-
            GE(x) |- (up_e, down_e)
            L1 and L2 are new labels
            ---------------------------------------------------- (Return)
            G(return x) |-  down_e ++ [ L1: R_ret <- up_e, L2: IReturn ]
    -}
    Ret x -> do
      (u, d) <- cogenExp (VarExp x)
      lbl1 <- newLabel
      lbl2 <- newLabel
      let r_ret = Regstr "_r_ret"
          i = IMove r_ret u
      return $ d ++ [(lbl1, i), (lbl2, IRet)]
    {-
            GE(cond) |- (up_cond, down_cond)
            LIf is a fresh label

            G(thn) |- instrs2
            LEndThen is a fresh label

            LElse is the next label (w/o incr)

            G(els) |- instrs3
            LEndElse is a fresh label

            LEndIf is the next label (w/o incr)
            instrs1 = [LIf: ifn up_cond goto LElse]
            instrs2' = instrs2 ++ [LEndThen: goto LEndIf]
            instrs3' = instrs3 ++ [LEndElse: goto LEndIf]
            ---------------------------------------------------- (If)
            G(if cond {thn} else {els}) |- down_cond ++ instrs1 ++ instrs2' ++ instrs3'
    -}
    If cond thn els -> do
      (cond_u, cond_d) <- cogenExp cond
      lblIf <- newLabel

      instrs2 <- cogen thn
      lblEThen <- newLabel

      lblElse <- chkNextLabel

      instrs3 <- cogen els
      lblEElse <- newLabel

      lblEIf <- chkNextLabel
      let instrs1 = [(lblIf, IIfNot cond_u lblElse)]
          instrs2' = instrs2 ++ [(lblEThen, IGoto lblEIf)]
          instrs3' = instrs3 ++ [(lblEElse, IGoto lblEIf)]
      return $ cond_d ++ instrs1 ++ instrs2' ++ instrs3'
    -- Lab 1 Task 2.2
    {-
            LBWhile is the next label (w/o incr)
            GE(cond) |- (up_cond, down_cond)

            LWhileCondJ is a fresh label
            G(body) |- instrs2
            LEndBody is a fresh label

            LEndWhile is the next label (w/o incr)

            instrs1 = [LWhileCondJ: ifn up_cond goto LEndWhile]
            instrs2' = instrs2 ++ [ LEndBody: goto LBWhile ]
            --------------------------------------------------------- (While)
            G(while cond {body}) |- down_cond ++ instrs1 ++ instrs2'

    -}
    While cond body -> undefined -- fixme

-- Lab 1 Task 2.2 end
