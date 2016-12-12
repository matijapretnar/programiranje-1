pozdravi :: IO ()
pozdravi =
    putStrLn "Kako ti je ime?" >>= \_ -> getLine >>= \ime -> putStrLn ("Živjo, " ++ ime ++ "!")
