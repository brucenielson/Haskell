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

foldIntList :: (Int -> IntList -> Int) -> Int -> IntList -> Int
foldIntList f acc Nil = acc
foldIntList f acc lst = f acc lst

lenConsCase :: Int -> IntList -> Int
lenConsCase acc Nil = acc
lenConsCase acc lst = 1 + acc

lenFoldList :: IntList -> Int
lenFoldList lst = foldIntList lenConsCase 0 lst



