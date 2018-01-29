leibniz_pi max =
    if (max < 0.0) then -1
    else
        let ref_pi = (4.0 * (atan 1.0)) in
        let calc_leibniz h = 4.0 * (-1.0) ** h / (2 * h + 1) in
        calc_delta (calc_leibniz 0) 0 where
            calc_delta i j =
                if (i - ref_pi >= 0.0) then
                    if (i - ref_pi <= max) then j
                    else calc_delta (i + (calc_leibniz (j + 1))) (j + 1)
                else 
                    if (i - ref_pi >= max) then j
                    else calc_delta (i + (calc_leibniz (j + 1))) (j + 1)
  
main = do
    putStrLn $ show $ leibniz_pi 0.01
    putStrLn $ show $ leibniz_pi 0.001