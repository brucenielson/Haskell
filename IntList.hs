-- Create new data type called IntList
-- It can have value of Nil or a type with parameters Int & IntList. i.e. _ :: Int -> IntList -> IntList
data IntList =  Nil | Cons Int IntList deriving (Show)

mkIntList :: [Int] -> IntList
mkIntList [] = Nil
mkIntList (x:xs) = Cons x (mkIntList xs)


list = mkIntList [1, 2, 3]

lenIntList :: IntList -> Int
lenIntList Nil = 0
lenIntList (Cons head tail) = 1 + lenIntList tail

-- len list
--why does this give an error when I make it it's own line in the file? 
--In Haskell do you have to only run these from the command line?

sumIntList :: IntList -> Int
sumIntList Nil = 0
sumIntList (Cons head tail) = head + sumIntList tail

foldIntList :: (IntList -> Int -> Int) -> Int -> IntList -> Int
foldIntList f acc Nil = acc
foldIntList f acc (Cons head tail) = f (Cons head Nil) (foldIntList f acc tail) 

lenConsCase :: IntList -> Int -> Int
lenConsCase lst acc = 1 + acc

sumConsCase :: IntList -> Int -> Int
sumConsCase Nil acc = acc
sumConsCase (Cons head tail) acc = head + acc

foldList :: IntList -> Int -> (IntList -> Int -> Int) -> Int
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
