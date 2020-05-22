-- Create new data type called IntList
-- It can have value of Nil or a type with parameters Int & IntList. i.e. _ :: Int -> IntList -> IntList
--data ConsCase = ConsCase { x :: Int, xs :: IntList} deriving (Show)
data ConsCase = ConsType Int IntList deriving (Show)
data IntList =  Nil | Cons ConsCase deriving (Show)

mkIntList :: [Int] -> IntList
mkIntList [] = Nil
mkIntList (x:xs) = Cons (ConsType x (mkIntList xs))


list = mkIntList [1, 2, 3]

-- Direct IntList Functions - Corresponds to Jerry Swan's Listing 2
lenIntList :: IntList -> Int
lenIntList Nil = 0
lenIntList xs = 1 + lenIntList xs

-- len list
--why does this give an error when I make it it's own line in the file? 
--In Haskell do you have to only run these from the command line?

sumIntList :: IntList -> Int
sumIntList Nil = 0
sumIntList (Cons cons) = consCaseHead cons + sumIntList (consCaseTail cons) 



-- New code required so that I can split a ConsCase into a head and tail.
-- I used to be able to just do this via pattern matching. Now that doesn't work because
-- "ConsCase" isn't an "IntType" and vice versa. 
consCaseHead :: ConsCase -> Int
consCaseHead (ConsType x _) = x

consCaseTail :: ConsCase -> IntList
consCaseTail (ConsType _ xs) = xs


-- Fold Functions - Corresponds to Jerry Swan's Listing 3
foldList :: IntList -> Int -> (ConsCase -> Int -> Int) -> Int
foldList Nil acc _ = acc
foldList (Cons xs) acc f = f (ConsType (consCaseHead xs) Nil) (foldList (consCaseTail xs) acc f)

lenConsCase :: ConsCase -> Int -> Int
lenConsCase _ acc = 1 + acc

sumConsCase :: ConsCase -> Int -> Int
sumConsCase cons acc = consCaseHead cons + acc


-- Examples of how to use
-- foldList list 0 sumConsCase
-- returns 6
-- foldList list 0 lenConsCase
-- returns 3


-- Final Functions using folds
length' :: IntList -> Int
length' lst = foldList lst 0 lenConsCase

sum' :: IntList -> Int
sum' lst = foldList lst 0 sumConsCase

-- Examples of how to use
-- length' list
-- returns 3
-- sum' list
-- returns 6
