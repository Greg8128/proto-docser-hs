
module Example where
import Data.Functor

-- | Some sample documentation for foo
foo = 4

data Bar a where
  -- | Constructor bar 1
  Bar1 :: String -> Bar Int
  Bar2 :: Int -> Bar Char

class Baz q where
  re :: Int -> q
  -- | 'im'
  im :: q -> Int
