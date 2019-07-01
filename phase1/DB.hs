module DB where

import Models.Entity
import Models
import Data.Maybe (listToMaybe, maybe)
import System.Directory

type Table a = [a]

-- Get an entity based its Id
getEntity :: Entity a => Int -> Table a -> Maybe a
getEntity id = listToMaybe . filter ((== id) . getId)

-- Insert an entity in a Table
insert :: Entity a => a -> Table a -> Table a
insert a t = either ((:) a) id . maybe (Left t) (Right . const t) $ getEntity (getId a) t

-- Delete an id from a Table
delete :: Entity a => Int -> Table a -> (Maybe a, Table a)
delete id = (,) <$> listToMaybe . filter ((== id) . getId) <*> filter ((/= id) . getId)  

-- Serialize a Table into a file
persist :: (Entity a, Show a) => FilePath -> Table a -> IO ()
persist file = writeFile file . show

-- Deserialize a Table from a file
load :: (Entity a, Read a) => FilePath -> IO (Table a)
load file = do
    r <- doesFileExist file
    if r 
      then do
        txt <- readFile file
        return . read $ txt
      else
        return []
