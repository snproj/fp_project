module Lang.Simp.Lattice.SignLattice where 

import Lang.Simp.Lattice.CompleteLattice

data SignAbsVal = Bot   -- ^ _|_
    | Minus             -- ^ -
    | Plus              -- ^ +
    | Top               -- ^ T
    | Zero              -- ^ 0
    deriving (Show, Eq, Ord)

-- Cohort Problem 10 Exercise 2
instance CompleteLattice SignAbsVal where 
    sqSubsetEq Bot _ = True
    sqSubsetEq _ Top = True
    sqSubsetEq x y = x == y

    lub Bot y = y
    lub x Bot = x
    lub Top _ = Top
    lub _ Top = Top
    lub x y | x == y = x
            | otherwise = Top
-- Cohort Problem 10 Exercise 2 End