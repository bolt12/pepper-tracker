module DB where

import           Models.Entity
import           Data.Maybe                     ( listToMaybe
                                                , maybe
                                                )
type Table a = [a]

-- Get an entity based its Id
getEntity :: Entity a => Int -> Table a -> Maybe a
getEntity id = listToMaybe . filter ((== id) . getId)

-- Insert an entity in a Table
insertEntity :: Entity a => a -> Table a -> Table a
insertEntity a t =
  either ((:) a) id . maybe (Left t) (Right . const t) $ getEntity (getId a) t

updateEntity :: Entity a => a -> Table a -> Table a
updateEntity _ [] = []
updateEntity a (h : t) | getId a == getId h = a : t
                       | otherwise          = h : updateEntity a t

-- Delete an id from a Table
deleteEntity :: Entity a => Int -> Table a -> (Maybe a, Table a)
deleteEntity id =
  (,) <$> listToMaybe . filter ((== id) . getId) <*> filter ((/= id) . getId)
