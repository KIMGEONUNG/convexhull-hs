module Utils where 

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

removeElement :: Eq a => a -> [a] -> [a]
removeElement _ [] = error "Fail : empty list"
removeElement target (x:[]) = if target == x then [] else error "Fail : Element not found"
removeElement target (x:xs) =
    if target /= x then [x] ++ removeElement target xs else xs

removeAllElement :: Eq a => a -> [a] -> [a] 
removeAllElement target [] = []  
removeAllElement target (x:xs) =
    if target == x 
        then removeAllElement target xs  
        else [x] ++ removeAllElement target xs  
