data Gender = M | F

ordinal :: Int -> String
ordinal 1 = "(first)"
ordinal 2 = "second"
ordinal 3 = "third"
ordinal n | n < 20 = (show n) ++ "th"
ordinal n | n `mod` 10 == 1 = (show n) ++ "st"
ordinal n | n `mod` 10 == 2 = (show n) ++ "nd"
ordinal n | n `mod` 10 == 3 = (show n) ++ "rd"
ordinal n = (show n) ++ "th"

multiple :: Int -> String
multiple 1 = "once"
multiple 2 = "twice"
multiple 3 = "thrice"
multiple n = (show n) ++ " times"

index :: Int -> Int -> (Maybe Gender) -> String

index 0 0 _ = "self"

-- Direct descendants
index 0 1 (Just M) = "son"
index 0 1 (Just F) = "daughter"
index 0 1 Nothing = "child"
index 0 2 gender = "grand" ++ (index 0 1 gender)
index 0 down gender = "great " ++ (index 0 (down-1) gender)

-- Direct ancestors
index 1 0 (Just M) = "father"
index 1 0 (Just F) = "mother"
index 1 0 Nothing = "parent"
index 2 0 gender = "grand" ++ (index 1 0 gender)
index up 0 gender = "great " ++ (index (up-1) 0 gender)

-- Siblings
index 1 1 (Just M) = "brother"
index 1 1 (Just F) = "sister"
index 1 1 Nothing = "sibling"

-- Descendants of siblings
index 1 2 (Just M) = "nephew"
index 1 2 (Just F) = "niece"
index 1 2 Nothing = (index 1 1 Nothing) ++ "'s " ++ (index 0 1 Nothing) ------
index 1 down gender = "great " ++ (index 1 (down-1) gender)

-- Siblings of ancestors
index 2 1 (Just M) = "uncle"
index 2 1 (Just F) = "aunt"
index 2 1 Nothing = (index 1 0 Nothing) ++ "'s " ++ (index 1 1 Nothing)
index up 1 gender = "great " ++ (index (up-1) 1 gender)

-- Miscellaneous relatives a.k.a. cousins
index up down _ | up == down = (ordinal (up-1)) ++ " cousin"
index up down gender | up > down = (index down down gender) ++ " " ++ (multiple (up-down)) ++ " removed"
index up down gender | up < down = index down up gender