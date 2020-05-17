--https://stackoverflow.com/questions/24884475/examples-of-histomorphisms-in-haskell

data Nat = S Nat | Z deriving (Show)


-- Allow us to construct Nats
mkNat :: Integer -> Nat
mkNat n | n < 0 = error "cannot construct negative natural number"
mkNat 0 = Z
mkNat n = S $ mkNat (n-1)

data NatF a = SF a | ZF -- Aside: this is just Maybe

cata :: (NatF a -> a)
    -> (Nat -> a)

cata f Z = f ZF -- No subterm to fold, base case
cata f (S subterm) = f $ SF $ cata f subterm -- Fold subterm first, recursive case

natToInteger :: Nat -> Integer
natToInteger = cata phi where
    -- We only need to provide a function to fold
    -- a non-recursive Nat-like structure
    phi :: NatF Integer -> Integer
    phi ZF = 0
    phi (SF x) = x + 1

-- We could use the type NatF (NonEmptyList a) here.
-- But because NatF is Maybe, NatF (NonEmptyList a) is equal to [a].
-- Using just [a] is a lot simpler
histo :: ([a] -> a)
    -> Nat -> a

histo f = head . go where
    -- go :: Nat -> [a]  -- This signature would need ScopedTVs
    go Z = [f []]
    go (S x) = let subvalues = go x in f subvalues : subvalues


-- Example: calculate the n-th fibonacci number
fibN :: Nat -> Integer
fibN = histo $ \x -> case x of
    (x:y:_) -> x + y
    _       -> 1

