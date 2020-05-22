-- Create new data type called IntList
-- It can have value of Nil or a type with parameters Int & IntList. i.e. _ :: Int -> IntList -> IntList
--data ConsCase = ConsCase { x :: Int, xs :: IntList} deriving (Show)
data ConsCase = ConsType Int IntList deriving (Show)
data IntList =  Nil | Cons ConsCase deriving (Show)

mkConsCase :: Int -> IntList -> ConsCase
mkConsCase x lst = ConsType x lst

mkIntList :: [Int] -> IntList
mkIntList [] = Nil
mkIntList (x:xs) = Cons (ConsType x (mkIntList xs))


list = mkIntList [1, 2, 3]

consCaseHead :: ConsCase -> Int
consCaseHead (ConsType x _) = x

consCaseTail :: ConsCase -> IntList
consCaseTail (ConsType _ xs) = xs



lenIntList :: IntList -> Int
lenIntList Nil = 0
lenIntList xs = 1 + lenIntList xs

-- len list
--why does this give an error when I make it it's own line in the file? 
--In Haskell do you have to only run these from the command line?

sumIntList :: IntList -> Int
sumIntList Nil = 0
sumIntList (Cons cons) = consCaseHead cons + sumIntList (consCaseTail cons) 

foldIntList :: (ConsCase -> Int -> Int) -> Int -> IntList -> Int
foldIntList _ acc Nil = acc
foldIntList f acc (Cons xs) = f (ConsType (consCaseHead xs) Nil) (foldIntList f acc (consCaseTail xs)) 

lenConsCase :: ConsCase -> Int -> Int
lenConsCase _ acc = 1 + acc

sumConsCase :: ConsCase -> Int -> Int
sumConsCase cons acc = consCaseHead cons + acc

foldList :: IntList -> Int -> (ConsCase -> Int -> Int) -> Int
foldList lst acc f = foldIntList f acc lst

-- foldList list 0 sumConsCase
-- returns 6
-- foldList list 0 lenConsCase
-- returns 3


length' :: IntList -> Int
length' lst = foldList lst 0 lenConsCase

sum' :: IntList -> Int
sum' lst = foldList lst 0 sumConsCase

-- length' list
-- returns 3
-- sum' list
-- returns 6
