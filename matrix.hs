
module Matrix where


data Matrix a = Matrix [[a]]

instance (Show a) => Show (Matrix a) where
  show (Matrix xxs) = show xxs


-- | Pipe argument to function
(.>) :: a -> (a->b) -> b
x .> f = f x


-- | Number of columns
ncol :: Matrix a -> Int
ncol (Matrix xxs) = length (xxs !! 0)

-- | Number of rows
nrow :: Matrix a -> Int
nrow (Matrix xxs) = length xxs

-- | Get nth column
colx :: Int -> Matrix a -> [a]
colx x (Matrix xxs) = map (!! x) xxs

-- | Get nth row
rowx :: Int -> Matrix a -> [a]
rowx x (Matrix xxs) = xxs !! x

-- | Transpose (rotate)
rot :: Matrix a -> Matrix a
rot m@(Matrix xxs) = Matrix [colx x m | x <- [0..(ncol m-1)]]

-- | Apply function to all items
app :: (a->b) -> Matrix a -> Matrix b
app f (Matrix xxs) = Matrix [[f x | x <- xs] | xs <- xxs]


-- | Null matrix
mat0 :: Int -> Int -> Matrix Int
mat0 colnum rownum = matx colnum rownum 0

-- | Matrix of ones
mat1 :: Int -> Int -> Matrix Int
mat1 colnum rownum = matx colnum rownum 1

-- | Matrix of _
matx :: Int -> Int -> a -> Matrix a
matx colnum rownum m = Matrix [[m | _ <- [1..colnum]] | _ <- [1..rownum]]

-- | Example matrix
m :: Matrix Int
m = Matrix [[1,2], [3,4], [5,6]]


