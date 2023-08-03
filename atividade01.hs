-- remove a letra inicial e final de cada palavra (duas implementações)
crop :: [x] -> [x]
crop list = if length list > 1 then reverse (init (reverse(init list))) else []

crop' :: [x] -> [x]
crop' [] = []
crop' [_] = []
crop' xs = reverse (init (reverse (init xs)))
