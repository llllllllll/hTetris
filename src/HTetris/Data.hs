-- |
-- Module      : HTetris.Data
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- All the data types to be used by the project.

module HTetris.Data where

import Control.Applicative ((<$>))
import Control.Arrow       ((***))
import Control.Monad       (liftM)
import Data.Function       (on)
import Graphics.Gloss      (Color,red,orange,yellow,cyan,blue,green,violet)
import System.Random       (getStdGen,randomRs)

-- -----------------------------------------------------------------------------
-- Blocks.

-- | A single game block with a location and a color.
data Block = Block { blockLocation :: Coord
		   , blockColor    :: Color
		   } deriving (Show)

-- | Equality relation is based only on location.
instance Eq Block where
    (==) a b = ((==) `on` blockLocation) a b

-- | The dimensions of one block in the game grid.
blockSize :: Float
blockSize = 20

-- -----------------------------------------------------------------------------
-- Tetrominos.

-- | Represents the type of tetromino.
data TetroType = L | J | I | O | S | Z | T deriving (Show,Eq,Enum)

-- | Represents a tetromino game peice.
data Tetromino = Tetromino { tetrominoType     :: TetroType
			   , tetrominoLocation :: Coord
			   , blocks            :: [Block]
                           }

-- | Equality relation is based only on the location.
instance Eq Tetromino where
    (==) a b = ((==) `on` tetrominoLocation) a b
               && ((==) `on` tetrominoType) a b

-- | Smart constructor for Tetromino from just TetroType t at Point (x,y).
mkTetromino :: TetroType -> Coord -> Tetromino
mkTetromino L l@(x,y) = Tetromino L l
                        $ map (flip Block orange)
                              [ (x - 1,y)
                              , l
                              , (x + 1,y)
                              , (x + 1,y)
                              ]
mkTetromino J l@(x,y) = Tetromino J l
                        $ map (flip Block blue)
                              [ (x - 1,y + 1)
                              , (x - 1,y)
                              , l
                              , (x + 1,y)
                              ]
mkTetromino I l@(x,y) = Tetromino I (x,y - 1)
                        $ map (flip Block cyan)
                              [ (x - 2,y)
                              , (x - 1,y)
                              , l
                              , (x + 1,y)
                              ]
mkTetromino O l@(x,y) = Tetromino O l
                        $ map (flip Block yellow)
                              [ l
                              , (x,y + 1)
                              , (x + 1,y + 1)
                              , (x + 1,y)
                              ]
mkTetromino S l@(x,y) = Tetromino S l
                        $ map (flip Block green)
                              [ (x - 1,y)
                              , l
                              , (x,y + 1)
                              , (x + 1,y + 1)
                              ]
mkTetromino Z l@(x,y) = Tetromino Z l
                        $ map (flip Block red)
                              [ (x - 1,y + 1)
                              , (x,y + 1)
                              , l
                              , (x + 1,y)
                              ]
mkTetromino T l@(x,y) = Tetromino T l
                        $ map (flip Block violet)
                              [ (x - 1,y)
                              , l
                              , (x,y + 1)
                              , (x + 1,y)
                              ]


-- -----------------------------------------------------------------------------
-- Misc.

-- | Represents a coordinate on the game grid.
type Coord = (Int,Int)

-- | Represents a shift direction for a block or tetromino.
data Shift = ShiftDown | ShiftRight | ShiftLeft deriving (Show, Eq)


data World = World { activeTetromino    :: Tetromino
		   , gameBlocks         :: [Block]
		   , upcomingTetrominos :: [TetroType]
                   , gameScore          :: Int
                   , lockTimer          :: Int
                   , worldStep          :: Int
		   }

-- | Delay in frames before a tetromino locks.
lockTime :: Int
lockTime = 30

-- | Where to spawn new upcoming tetrominos.
spawnLoc :: Coord
spawnLoc = (4,20)

getRandomTypes :: IO [TetroType]
getRandomTypes = map intToType
                 <$> liftM (randomRs (0 :: Int,6 :: Int)) getStdGen
  where
      intToType c
          | c == 0 = L
          | c == 1 = J
          | c == 2 = I
          | c == 3 = O
          | c == 4 = S
          | c == 5 = Z
          | c == 6 = T

-- | A fresh game world with only 1 Tetromino at sPAWN and no game blocks
newWorld :: [TetroType] -> World
newWorld ts = World { activeTetromino    = mkTetromino (head ts) spawnLoc
                    , gameBlocks         = []
                    , upcomingTetrominos = tail ts
                    , gameScore          = 0
                    , lockTimer          = lockTime
                    , worldStep          = 0
                    }
