main = do
        l <- readInts
        print (scoreTable l i  ++ show (pointCounter l i))



readInts :: IO [Int]
readInts = fmap (map read.words) getLine

i :: Int
i=0


pointCounter :: (Eq a, Num a) => [a] -> a -> a
pointCounter [] i = 0
pointCounter [a] i = a
pointCounter [a,b,c] i = if i==9 then a+b+c else a+2*(b+c)
pointCounter [a,b] i = a+b
pointCounter (a:b:c:t) i
    | a==10 = sum [a,b,c] + pointCounter (b:c:t) (i+1)
    | a+b==10 = sum [a,b,c] + pointCounter (c:t) (i+1)
    | otherwise = sum [a,b] + pointCounter (c:t) (i+1)


scoreTable :: (Eq a, Num a, Show a) => [a] -> a -> String
scoreTable [] i = " "
scoreTable [a,b,c] i
    | i==9 && a==10 && b==10 && c==10 = "X X X | "
    | i==9 && a==10 && b==10 = "X X " ++ show c ++ " | "
    | i==9 && a==10 && b+c==10 = "X " ++ show b ++ " / | "
    | i==9 && a==10 = "X " ++ show b ++ " " ++ show c ++ " | "
    | i==9 && a+b==10 && c== 10 = show a ++ " / X | "
    | i==9 = show a ++ " / " ++ show c ++ " | "
    | otherwise = "X _ | " ++ scoreTable [b,c] i
scoreTable [a,b] i
    | a+b==10 = show a ++ " / | "
    | otherwise = show a ++ " " ++ show b ++ " | "
scoreTable (a:b:t) i
    | a==10 = "X _ | " ++ scoreTable (b:t) (i+1)
    | otherwise = scoreTable [a,b] i ++ scoreTable t  (i+1)
scoreTable [a] i = " "