findNegative :: [Integer] -> Int -> Int
findNegative [] _ = -1
findNegative (x:xs) i	| x < 0 = i
			| otherwise = findNegative xs (i+1)

unpackFirst :: (a,a) -> a
unpackFirst (x,_) = x

unpackSecond :: (a,a) -> a
unpackSecond (_,x) = x

splitRemove :: Int -> [Integer] -> [[Integer]]
splitRemove 0 xs = splitLists $ tail xs
splitRemove i xs = [sub]++(splitLists remainder)
    where   sub = unpackFirst slist
	    remainder = unpackSecond $ splitAt 1 $ unpackSecond slist
	    slist = splitAt i xs

splitLists :: [Integer] -> [[Integer]]
splitLists x	| splitVal >= 0 = splitRemove splitVal x
		| otherwise = [x]
		    where splitVal = findNegative x 0

sumLists :: [[Integer]] -> [Integer]
sumLists x = map (sum) x

maxContSum :: [Integer] -> Integer
maxContSum x = maximum $ sumLists $ splitLists x

main = do

print "Largest Contiguous Sum"
print $ maxContSum [0,-3,2,1,4,5,9,-3,2]
