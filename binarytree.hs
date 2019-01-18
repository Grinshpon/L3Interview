data Bin a = End | Node a (Bin a) (Bin a) deriving(Ord, Eq, Show)

binValue :: Bin a -> a
binValue (Node x _ _) = x

insert :: (Ord a) => a -> Bin a -> Bin a
insert x End = Node x End End
insert x (Node val (leftChild) (rightChild))	| x > val = Node val (leftChild) (insert x rightChild)
						| x < val = Node val (insert x leftChild) (rightChild)
						| otherwise = Node val (leftChild) (rightChild)
-- The reason I check for < and > both is to check if the value to be inserted is a duplicate, and do nothing if it is

testTree :: Bin Int -- example to test functionality
testTree = Node 8
    (Node 5
      (Node 1
        End
        End
      )
      (Node 6
        End
        End
      )
    )
    (Node 7
      End
      End
    )

main = do

print "test"

let x = insert 9 testTree

print $ show $ insert 4 x
