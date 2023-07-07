
module Data.Ordinal where

import Data.Kind (Type)
import Data.Type.Equality (type (:~:) (Refl))

data Tree = Leaf | Node Tree Tree
    deriving (Show, Eq, Ord)

data Lt :: Tree -> Tree -> Type where
    Lt₁ :: Lt 'Leaf ('Node a b)
    Lt₂ :: Lt a c -> Lt ('Node a b) ('Node c d)
    Lt₃ :: Lt b c -> Lt ('Node a b) ('Node a c)

type Gt a b = Lt b a

type Ge a b = Either (Gt a b) (a :~: b)

type family Fst t :: Tree where
    Fst 'Leaf = 'Leaf
    Fst ('Node a _) = a

data CNF :: Tree -> Type where
    CNFLeaf :: CNF 'Leaf
    CNFNode :: CNF a -> CNF b -> Ge a (Fst b) -> CNF ('Node a b)

type Zero  = 'Leaf
type One   = 'Node 'Leaf Zero
type Two   = 'Node 'Leaf One
type Three = 'Node 'Leaf Two

zero :: CNF Zero
zero = CNFLeaf

one :: CNF One
one = CNFNode zero zero (Right Refl)

two :: CNF Two
two = CNFNode zero one (Right Refl)

-- data Tree = Zero | OmegaPlus Tree Tree

-- data STree a where
--     SZero      :: STree 'Zero
--     SOmegaPlus :: STree a -> STree b -> STree ('OmegaPlus a b)

-- data Lt a b where
--     LtZ :: Lt 'Zero ('OmegaPlus a b)
--     LtP :: Lt a c -> Lt ('OmegaPlus a b) ('OmegaPlus c d)
--     LtB :: Lt b d -> Lt ('OmegaPlus a b) ('OmegaPlus a d)

-- data Ge a b where
--     GeE :: Ge a a
--     GeN :: Lt b a -> Ge a b

-- type family Fst t where
--     Fst 'Zero = 'Zero
--     Fst ('OmegaPlus a b) = a

-- data CNF t where
--     CNFZ :: CNF 'Zero
--     CNFO :: CNF a -> CNF b -> Ge a (Fst b) -> CNF ('OmegaPlus a b)

-- type One = 'OmegaPlus 'Zero 'Zero

-- zero :: CNF 'Zero
-- zero = CNFZ

-- one :: CNF One
-- one = CNFO zero zero GeE

-- omega :: CNF ('OmegaPlus One 'Zero)
-- omega = CNFO one zero (GeN LtZ)

-- tri :: CNF a -> CNF b -> Either (Lt a b) (Ge a b)
-- tri CNFZ CNFZ = Right GeE
-- tri (CNFO _ _ _) CNFZ = Right (GeN LtZ)
-- tri CNFZ (CNFO _ _ _) = Left LtZ
-- tri (CNFO a c _) (CNFO b d _) =
--     case tri a b of
--         Left r -> Left (LtP r)
--         Right (GeN r) -> Right (GeN (LtP r))
--         Right GeE ->
--             case tri c d of
--                 Left r -> Left (LtB r)
--                 Right (GeN r) -> Right (GeN (LtB r))
--                 Right GeE -> Right GeE

-- type family AddTree a b where
--     AddTree 'Zero b = b
--     AddTree a 'Zero = a
    -- AddTree ('OmegaPlus a c) ('OmegaPlus b d) =
        -- If (Lt a b) ('OmegaPlus b d) ('OmegaPlus a (AddTree c ('OmegaPlus b d)))
