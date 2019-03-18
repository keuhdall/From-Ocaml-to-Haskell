hfs_f :: Int -> Int
hfs_f n
    | n < 0 = -1
    | n == 0 = 1
    | otherwise = n - (hfs_m $ hfs_f $ n - 1)

hfs_m :: Int -> Int
hfs_m n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = n - (hfs_f $ hfs_m $ n - 1)
  
main = do
    putStrLn $ show $ hfs_m 0
    putStrLn $ show $ hfs_f 0
    putStrLn $ show $ hfs_m 4
    putStrLn $ show $ hfs_f 4