-- Name: Carter Mooring
-- File: hw9b.hs
-- Desc: This file contains Haskell functions written to make use of 
--          Pattern Matching and Guards. These functions perform the same 
--          tasks as the functions in hw9a.hs
--


-- Place answers below. Be sure to include comments for each function
-- (with the question number and a short description). Also include
-- explicit type declarations for each function.


--Q(1) Write a function myMinimum that returns the smallest of a given 
--      list of values. Example: myMinimum [7,1,9,12,10] should return 1. 
--      Note the function should return an exception (error) when called 
--      on an empty list. Be careful with respect to eficiency, i.e., your 
--      implementation must be O(n) for an n-element list.
--
myMinimum :: [Int] -> Int
myMinimum [] = error "Error: Empty List"
myMinimum [x] = x
myMinimum (x:xs)
          | x < m = x
          | otherwise = m 
            where m = myMinimum xs


--Q(2) Write a function myReverse that takes a list and returns the reverse 
--      order of the list. Example: myReverse [1,2,3] should return [3,2,1]. 
--      Note that this function does not require guards.
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse xs = [last xs] ++ myReverse (init xs)


--Q(3) Write a function myLength that gives the length of a list. Example: 
--      myLength [1,3,5] should return 3. Note that this function does not require guards.
--
myLength :: Num p => [a] -> p
myLength [] = 0
myLength [x] = 1
myLength xs = 1 + myLength (tail xs)


--Q(4) Write a function myElement that takes a value and a list and returns true 
--      if the value is in the list, and false otherwise. Examples: myElement 3 [1,2,3,4] 
--      should return true whereas myElement 3 [1,2,4,5] should return false.
--
myElement :: Eq a => a -> [a] -> Bool
myElement x [] = False
myElement x xs | (head xs) == x = True
            | otherwise = myElement x (tail xs)


--Q(5) Write a function myElements that takes two lists of values and returns true if 
--      all the values in the 􏰅rst list are in the second list. Examples: 
--      myElements "db" "abcd" should return true whereas myElements [1,2] [0,1,3,4] should return false. 
--      Trivially, myElements [] [1,2,3,4,5] is true. Note you can call myElement from within myElements.
--
myElements :: Eq a => [a] -> [a] -> Bool
myElements [] ys = True
myElements xs ys | myElement (head xs) ys = myElements (tail xs) ys
                 | otherwise = False


--Q(6) Write a function myReplace that takes a pair of values and a list and returns a new list 
--      such that each occurrence of the 􏰅rst value of the pair in the list is replaced with the 
--      second value. Example: myReplace (2,8) [1,2,3,2] should return [1,8,3,8].
--
myReplace :: Eq a => (a, a) -> [a] -> [a]
myReplace (x, y) [] = []
myReplace (x, y) xs | x == (head xs) = y : myReplace (x, y) (tail xs)
                    | otherwise = (head xs) : myReplace (x, y) (tail xs)


--Q(7) Write a function myReplaceAll that takes a list of pairs and a list of values and returns a 
--      new list where each occurrence of the 􏰅rst value in a pair is replaced by the second value 
--      in the pair. The replacement should occur in order of pairs. 
--      Examples: myReplaceAll [('a','b'), ('c','d')] "abcd" should give "bbdd" and 
--      myReplaceAll [(1,2), (2,3)] [1,2,3,4] should give [3,3,3,4]. You can call myReplace from 
--      within myReplaceAll. Note also that you do not need guards to de􏰅ne this function.
--
myReplaceAll :: Eq a => [(a, a)] -> [a] -> [a]
myReplaceAll rs [] = error "No list exists to replace"
myReplaceAll [] xs = xs
myReplaceAll rs xs = myReplaceAll (tail rs) (myReplace (head rs) xs)


--Q(8) Write a function myElementSum that takes a value and a list, and returns the sum of the given 
--      values in the list. Examples: myElementSum 10 [15,10,25] should return 10, myElementSum 3 [3,2,3,2,3,4,3] 
--      should give 12 and myElementSum 3 [] should give 0.
--
myElementSum :: (Num a, Eq a) => a -> [a] -> a
myElementSum x [] = 0
myElementSum x xs | not (myElement x xs) = 0
                  | (head xs) == x = x + (myElementSum x (tail xs))
                  | otherwise = (myElementSum x (tail xs))


--Q(9) Write a function removeDuplicates that takes a list of values, and returns the original list with duplicate 
--      values removed. Examples: removeDuplicates ['a','b','a','c','b','a'] should return ['c','b','a'] and 
--      removeDuplicates [10,11,13,11,12] should return [10,13,11,12]. Note you can call myElement within your removeDuplicates function.
--
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates xs | (myElement (head xs) (tail xs)) = (removeDuplicates (tail xs))
                    | otherwise = (head xs) : removeDuplicates (tail xs)

--Q(10) Write a mergeSort function that takes a list of pairs and sorts the list on the 􏰅rst element of the pair 
--      using the merge sort algorithm. For example, mergeSort [(2,10), (1,15), (4,30)] should return [(1,15), (2,10), (4,30)] 
--      and mergeSort [("b",40), ("c",20),("a",30),("d",10)] should return [("a",30),("b",40), ("c",20),("d",10)]. Note that you 
--      can use the div function to perform integer division (e.g., div 5 2 evaluates to 2􏰃alternatively, you can write 5 `div` 2).
--
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] b = b
merge a [] = a
merge (x:xs) (y:ys) 
    |(x <= y)  = x:(merge xs (y:ys)) 
    |otherwise = y:(merge (x:xs) ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort a) (mergeSort b)
            where 
                a = take (div (length xs) 2) xs
                b = drop(div (length xs) 2) xs
