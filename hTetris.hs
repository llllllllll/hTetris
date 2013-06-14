module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans
import Data.Array

data World = World { active_tetr :: Tetromino
					,inactive_blocks :: [Block]
					} deriving (Show)


new_world :: World
new_world = World (mk_tetromino T) []

data TetroType = L | J | I | O | S | Z | T deriving (Show, Eq)

data Tetromino = Tetromino { tetromino_type :: TetroType
					,tetromino_location :: Point
					,blocks :: [Block]
				   } deriving (Show, Eq)

data Block = Block { block_open :: Bool
					,block_location :: Point
					,block_color :: Color
				   } deriving (Show, Eq)

-- The dimensions of one block in the game grid
bLOCK_SIZE :: Float
bLOCK_SIZE = 20

--The offset of the game grid from (0,0)
gRID_OFFSET :: (Float,Float)
gRID_OFFSET = (0,0)

--A 2d Array representing the 10x22 game board the game is played in
gRID :: Array (Int,Int) (Float,Float)
gRID = array ((0,0),(9,21)) [((r,c),(bLOCK_SIZE*fromIntegral r,bLOCK_SIZE*fromIntegral c)) | c <- [0..21], r <- [0..9]]

-- Converts Tetromino t to a Picture
paint_tetromino :: Tetromino -> Picture
paint_tetromino t = Pictures [Translate ((fst . tetromino_location) t) ((snd . tetromino_location) t) (paint_block bl) | bl <- blocks t]

-- Converts Block b to a Picture
paint_block :: Block -> Picture
paint_block b = Color (block_color b) (Polygon [(x,y),(x+bLOCK_SIZE,y),(x+bLOCK_SIZE,y-bLOCK_SIZE),(x,y-bLOCK_SIZE)])
	where
		y = bLOCK_SIZE * (snd . block_location) b
		x = bLOCK_SIZE * (fst . block_location) b

-- Makes a Tetromino from just TetroType t in the Tetronimo spawn location
mk_tetromino :: TetroType -> Tetromino
mk_tetromino t
	| t == L = Tetromino L (gRID!(0,0)) [Block False (-1,0) orange, Block False (0,0) orange, Block False (1,0) orange, Block False (1,1) orange]
	| t == J = Tetromino J (gRID!(0,0)) [Block False (-1,1) blue, Block False (-1,0) blue, Block False (0,0) blue, Block False (1,0) blue]
	| t == I = Tetromino I (gRID!(0,0)) [Block False (-2,0) cyan, Block False (-1,0) cyan, Block False (0,0) cyan, Block False (1,0) cyan]
	| t == O = Tetromino O (gRID!(0,0)) [Block False (0,0) yellow, Block False (0,1) yellow, Block False (1,1) yellow, Block False (1,0) yellow]
	| t == S = Tetromino S (gRID!(0,0)) [Block False (-1,0) green, Block False (0,0) green, Block False (0,1) green, Block False (1,1) green]
	| t == Z = Tetromino Z (gRID!(0,0)) [Block False (-1,1) red, Block False (0,1) red, Block False (0,0) red, Block False (1,0) red]
	| t == T = Tetromino T (gRID!(0,0)) [Block False (-1,0) violet, Block False (0,0) violet, Block False (0,1) violet, Block False (1,0) violet]

-- Translates teromino t to point (x,y)
translate_tetromino :: Tetromino -> Point -> Tetromino
translate_tetromino b (x,y) = Tetromino (tetromino_type b) (x,y) (blocks b)

-- Rotates Tetromino t -pi/2
rotate_tetromino :: Tetromino -> Tetromino
rotate_tetromino t 
	| tetromino_type t == O = t
	| otherwise = Tetromino (tetromino_type t) (tetromino_location t) 
			[Block False (-(snd . block_location) bl,(fst . block_location) bl) (block_color bl) | bl <- blocks t]

-- Generates a new Tetromino from a string and returns the rotated string
next_tetromino :: String -> (Tetromino,String)
next_tetromino (n:ns)
	| n == 'L' = (mk_tetromino L,ns ++ [n])
	| n == 'J' = (mk_tetromino J,ns ++ [n])
	| n == 'I' = (mk_tetromino I,ns ++ [n])
	| n == 'O' = (mk_tetromino O,ns ++ [n])
	| n == 'S' = (mk_tetromino S,ns ++ [n])
	| n == 'Z' = (mk_tetromino Z,ns ++ [n])
	| n == 'T' = (mk_tetromino T,ns ++ [n])
 
main :: IO ()
main = play (InWindow "hTetris" (400,600) (10,10)) white 1 new_world paint_world get_input next_frame

paint_world :: World -> Picture
paint_world w = Pictures $ (paint_tetromino (active_tetr w)):[paint_block bl | bl <- inactive_blocks w]

next_frame :: Float -> World -> World
next_frame _ w = World (new_tetr w) (inactive_blocks w)
	where
		new_tetr w = active_tetr w
		--new_tetr w = translate_tetromino (active_tetr w) ((fst . tetromino_location) (active_tetr w),(snd . tetromino_location) (active_tetr w) - bLOCK_SIZE)
		--new_tetr w = rotate_tetromino (active_tetr w)

get_input :: Event -> World -> World
get_input e w = w