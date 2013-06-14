-- Functions and a data type to work with game blocks
module Tetromino.Block
	( Block(..)
	, bLOCK_SIZE
	, gRID
	, gRID_OFFSET
	, paint_block
	, block_extent ) where
		
import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game

data Block = Block { block_location :: Coord
					,block_color :: Color
				   } deriving (Show, Eq)

-- The dimensions of one block in the game gRID
bLOCK_SIZE :: Float
bLOCK_SIZE = 20

--A 2d Array representing the 10x22 game board the game is played in
gRID :: Array Coord Point
gRID = array ((0,0),(9,21)) [((r,c),(bLOCK_SIZE * fromIntegral r + fst gRID_OFFSET,bLOCK_SIZE * fromIntegral c + fst gRID_OFFSET)) | c <- [0..21], r <- [0..9]]

--The offset of the game gRID from (0,0)
gRID_OFFSET :: Point
gRID_OFFSET = (-50,-150)

-- Converts Block b to a Picture
paint_block :: Block -> Picture
paint_block b = let (x,y) = gRID!((fst . block_location) b,(snd . block_location) b) in
			Color (block_color b) (Polygon [(x,y),(x+bLOCK_SIZE,y),(x+bLOCK_SIZE,y-bLOCK_SIZE),(x,y-bLOCK_SIZE)])

-- Gets the bounding box of block b
block_extent :: Block -> Extent
block_extent b = let (x,y) = block_location b in makeExtent (y) (y-round bLOCK_SIZE) (x) (x-round bLOCK_SIZE)
