-- "One way or another" (c) by Ignacio Slater M.

-- "One way or another" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

module FindEither where

type Assoc k v = [(k, v)]
type Error = String

{-|
  Finds a key in an associative table or returns an Error in case it wasn't found.

  >>> find 2 [(2, 'a'), (1, 'b')]
  Right 'a'
  >>> find 2 [(2, 'a'), (1, 'b'), (2, 'a')]
  Right 'a'
  >>> find 3 [(2, 'a'), (1, 'b')]
  Left "Key not found"
  >>> find 2 [(2, 'a'), (1, 'b'), (2, 'c')]
  Left "Multiple values for key 2"
-}
find :: (Eq k, Show k, Eq v) => k -> Assoc k v -> Either Error v
find query table = if null table
  then notFound
  else foldr
    (\t acc -> if fst t == query
      then if isRight acc && (val acc /= snd t)
        then Left ("Multiple values for key " ++ show (fst t))
        else Right (snd t)
      else acc
    )
    notFound
    table
 where
  notFound = Left "Key not found"
  isRight (Right _) = True
  isRight _         = False
  val (Right x) = x
  val (Left  _) = error "Can't get value of error type"
