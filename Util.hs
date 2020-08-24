removeAllElement :: Eq a => a -> [a] -> [a]
removeAllElement target [] = [] 
removeAllElement target (x:xs) =
    if target == x 
        then removeAllElement target xs 
        else [x] ++ removeAllElement target xs 
    
