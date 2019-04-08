leibniz_pi :: Float -> Float
leibniz_pi max
    | max < 0.0 = -1
    | otherwise = calc_delta (calc_leibniz 0) 0 where
        ref_pi = 4.0 * (atan 1.0)
        calc_leibniz h = 4.0 * (-1.0) ** h / (2 * h + 1)
        calc_delta i j
            | (i - ref_pi >= 0.0 && i - ref_pi <= max)  = j
            | (i - ref_pi >= 0.0 && i - ref_pi > max)   = calc_delta (i + (calc_leibniz $ j+1)) (j+1)
            | (i - ref_pi >= max)                       = j
            | otherwise                                 = calc_delta (i + (calc_leibniz $ j+1)) (j+1)
main = do
    putStrLn $ show $ leibniz_pi 0.01
    putStrLn $ show $ leibniz_pi 0.001