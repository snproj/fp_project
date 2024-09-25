module Lang.Simp.Lattice.SignLattice where 

import Lang.Simp.Lattice.CompleteLattice

data SignAbsVal = Bot   -- ^ _|_
    | Minus             -- ^ -
    | Plus              -- ^ +
    | Top               -- ^ T
    | Zero              -- ^ 0
    deriving (Show, Eq, Ord)


instance CompleteLattice SignAbsVal where 
    sqSubsetEq Bot _        = True
    sqSubsetEq Top Top      = True
    sqSubsetEq Top _        = False
    sqSubsetEq Plus Bot     = False
    sqSubsetEq Plus Plus    = True
    sqSubsetEq Plus _       = False
    sqSubsetEq Minus Bot    = False
    sqSubsetEq Minus Minus  = True
    sqSubsetEq Minus Top    = True 
    sqSubsetEq Minus _      = False
    sqSubsetEq Zero Bot     = False
    sqSubsetEq Zero Zero    = True
    sqSubsetEq Zero Top     = True
    sqSubsetEq Zero _       = False 
    lub Bot b       = b
    lub Top _       = Top
    lub a Bot       = a
    lub Plus Plus   = Plus
    lub Plus _      = Top
    lub Minus Minus = Minus
    lub Minus _     = Top
    lub Zero Zero   = Zero 
    lub Zero _      = Top 
