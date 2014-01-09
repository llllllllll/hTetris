-- |
-- Module      : HTetris.Tetromino.Block
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions that act on or with 'Block's.

module HTetris.Tetromino.Block
	( grid           -- :: Array Coord Point
	, gridOffset     -- :: Point
	, paintBlock     -- :: Block -> Picture.
        , translateBlock -- :: Shift -> Block -> Block
        ) where

import HTetris.Data

import Control.Arrow  ((***))
import Data.Array     (Array,(!),array)
import Graphics.Gloss (Picture(..),Point)



-- | A 2d Array representing the 10x22 game board the game is played in.
grid :: Array Coord Point
grid = array ((0,0),(9,21)) [ ((x,y)
                              ,((+) (fromIntegral x * blockSize) ***
                                (+) (fromIntegral y * blockSize))
                                gridOffset)
                             | x <- [0..9], y <- [0..21]]

-- | The offset of the game grid from (0,0).
gridOffset :: Point
gridOffset = (-100,-150)

-- | Converts Block b to a Picture.
paintBlock :: Block -> Picture
paintBlock b = let (x,y) = grid ! blockLocation b
               in Color (blockColor b) $ Polygon [ (x,y)
                                                 , (x + blockSize,y)
                                                 , (x + blockSize,y - blockSize)
                                                 , (x,y - blockSize)
                                                 ]

-- | Translates a block 1 unit in the Shift direction.
translateBlock :: Shift -> Block -> Block
translateBlock ShiftDown  b = b { blockLocation = (id *** flip (-) 1)
                                                  $ blockLocation b
                                }
translateBlock ShiftLeft  b = b { blockLocation = (flip (-) 1 *** id)
                                                  $ blockLocation b
                                }
translateBlock ShiftRight b = b { blockLocation = ((+) 1 *** id)
                                                  $ blockLocation b
                                }
