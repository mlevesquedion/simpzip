{-# LANGUAGE TypeSynonymInstances #-}

data Zipper a = Zipper ([a], [a]) deriving (Eq, Ord, Show)

instance Functor Zipper where
  fmap f (Zipper (before, after)) = Zipper (map f before, map f after)

toList :: Zipper a -> [a]
toList (Zipper (before, after)) = reverse before ++ after

fromList :: [a] -> Zipper a
fromList xs = Zipper ([], xs)

right :: Zipper a -> Zipper a
right z@(Zipper (_, []))         = z
right (Zipper (before, x:after)) = Zipper (x:before, after)

left :: Zipper a -> Zipper a
left z@(Zipper ([], _))         = z
left (Zipper (x:before, after)) = Zipper (before, x:after)

(|>) :: a -> (a -> b) -> b
x |> f = f x

z :: Zipper Int
z = fromList [1..3]
    |> right
    |> right
    |> right
    |> right
    |> left
    |> fmap (+1)

main :: IO ()
main = do
  putStrLn $ show z
  putStrLn $ show $ toList z
