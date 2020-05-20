-- Create new data type called IntList
-- It can have value of Nil or a type with parameters Int & IntList. i.e. _ :: Int -> IntList -> IntList
data IntList =  Nil | Cons Int IntList deriving (Show)

mkIntList :: [Int] -> IntList
mkIntList [] = Nil
mkIntList (x:xs) = Cons x (mkIntList xs)


list = mkIntList [1, 2, 3]

lenIntList :: IntList -> Int
lenIntList Nil = 0
lenIntList (Cons _ xs) = 1 + lenIntList xs

-- len list
--why does this give an error when I make it it's own line in the file? 
--In Haskell do you have to only run these from the command line?

sumIntList :: IntList -> Int
sumIntList Nil = 0
sumIntList (Cons x xs) = x + sumIntList xs

foldIntList :: (IntList -> Int -> Int) -> Int -> IntList -> Int
foldIntList _ acc Nil = acc
foldIntList f acc (Cons x xs) = f (Cons x Nil) (foldIntList f acc xs) 

lenConsCase :: IntList -> Int -> Int
lenConsCase _ acc = 1 + acc

sumConsCase :: IntList -> Int -> Int
sumConsCase Nil _ = error "Can't call sumConsCase with a Nil value"
sumConsCase (Cons x xs) acc = x + acc

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
