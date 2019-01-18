import Data.Word

data IPv4 = IPv4 Word8 Word8 Word8 Word8

showIP :: IPv4 -> String
showIP (IPv4 w x y z) = ""++(show w)++"."++(show x)++"."++(show y)++"."++(show z)

makeIP :: Int -> Int -> Int -> Int -> IPv4
makeIP w x y z = (IPv4 (w::Word8) (x::Word8) (y::Word8) (z::Word8))

possibleAddresses :: String -> [IPv4]
possibleAddresses (w:x:y:z:[]) = [makeIP (toInt w) (toInt x) (toInt y) (toInt z)]

foo :: IPv4 -> Int
foo x = 1

main = do

print "test"
