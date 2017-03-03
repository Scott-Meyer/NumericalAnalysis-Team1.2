--stuff
main :: IO ()
main = putStrLn "Stuffs"

gaussian :: (Float -> Float) -> Float -> Float
gaussian f x = f x