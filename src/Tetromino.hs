-- Functions and a data type to represent things about the Tetromino type
module Tetromino
	( TetroType(..)
	, Tetromino(..)
	, mk_tetromino
	, tetromino_extent
	, paint_tetromino
	, translate_tetromino
	, rotate_tetromino ) where

import Tetromino.Block
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game

-- Represents the type of tetromino
data TetroType = L | J | I | O | S | Z | T deriving (Show, Eq)

-- Represents a tetromino game peice 
data Tetromino = Tetromino { tetromino_type :: TetroType
					,tetromino_location :: Coord
					,blocks :: [Block]
				   } deriving (Show, Eq)

-- Makes a Tetromino from just TetroType t at Point (x,y)
mk_tetromino :: TetroType -> Coord -> Tetromino
mk_tetromino t (x,y)
	| t == L = Tetromino L (x,y) [Block (x-1,y) orange, Block (x,y) orange, Block (x+1,y) orange, Block (x+1,y+1) orange]
	| t == J = Tetromino J (x,y) [Block (x-1,y+1) blue, Block (x-1,y) blue, Block (x,y) blue, Block (x+1,y) blue]
	| t == I = Tetromino I (x,y) [Block (x-2,y) cyan, Block (x-1,y) cyan, Block (x,y) cyan, Block (x+1,y) cyan]
	| t == O = Tetromino O (x,y) [Block (x,y) yellow, Block (x,y+1) yellow, Block (x+1,y+1) yellow, Block (x+1,y) yellow]
	| t == S = Tetromino S (x,y) [Block (x-1,y) green, Block (x,y) green, Block (x,y+1) green, Block (x+1,y+1) green]
	| t == Z = Tetromino Z (x,y) [Block (x-1,y+1) red, Block (x,y+1) red, Block (x,y) red, Block (x+1,y) red]
	| t == T = Tetromino T (x,y) [Block (x-1,y) violet, Block (x,y) violet, Block (x,y+1) violet, Block (x+1,y) violet]

-- Gets the bounding boxes of Tetromino t
tetromino_extent :: Tetromino -> [Extent]
tetromino_extent t = map block_extent (blocks t)

-- Converts Tetromino t to a Picture
paint_tetromino :: Tetromino -> Picture
paint_tetromino t = Pictures [paint_block bl | bl <- blocks t]

-- Translates teromino t to point (x,y)
translate_tetromino :: Tetromino -> Coord -> Tetromino
translate_tetromino t (x,y) = Tetromino (tetromino_type t) (x,y) (map (translate_block (x,y)) (blocks t))
	where
		translate_block (x',y') b = let (x,y) = block_location b in Block (2*x-x',2*y-y') (block_color b)

-- Rotates Tetromino t -pi/2
rotate_tetromino :: Tetromino -> Tetromino
rotate_tetromino t 
	| tetromino_type t == O = t
	| otherwise = Tetromino (tetromino_type t) (tetromino_location t) 
			[Block ((fst . tetromino_location) t + (snd . tetromino_location) t - (snd . block_location) bl,
			-(fst . tetromino_location) t + (snd . tetromino_location) t + (fst . block_location) bl) 
			(block_color bl) | bl <- blocks t]