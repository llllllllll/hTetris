module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans
import Data.Array

data World = World { active_tetr :: Tetromino
					,inactive_blocks :: [Block]
					} deriving (Show)


new_world :: World
new_world = World (mk_tetromino L) []

data TetroType = L | J | I | O | S | Z | T deriving (Show, Eq)

data Tetromino = Tetromino { tetromino_type :: TetroType
					,tetromino_location :: Point
					,blocks :: [Block]
				   } deriving (Show, Eq)

data Block = Block { block_location :: Point
					,block_color :: Color
				   } deriving (Show, Eq)

-- The dimensions of one block in the game gRID
bLOCK_SIZE :: Float
bLOCK_SIZE = 20

--The offset of the game gRID from (0,0)
gRID_OFFSET :: (Float,Float)
gRID_OFFSET = (0,0)

--A 2d Array representing the 10x22 game board the game is played in
gRID :: Array (Int,Int) (Float,Float)
gRID = array ((0,0),(9,21)) [((r,c),(bLOCK_SIZE*fromIntegral r,bLOCK_SIZE*fromIntegral c)) | c <- [0..21], r <- [0..9]]

-- Converts Tetromino t to a Picture
paint_tetromino :: Tetromino -> Picture
paint_tetromino t = Pictures [Translate ((fst . tetromino_location) t) ((snd . tetromino_location) t) (paint_block bl) | bl <- blocks t]

extent_tetromino :: Tetromino -> [Extent]
extent_tetromino t = map (extent_block . normalize_block t) (blocks t)

normalize_block :: Tetromino -> Block -> Block
normalize_block t b = let (x,y) = block_location b in let (x',y') = tetromino_location t in Block (x'+x,y'+y) (block_color b)

-- Converts Block b to a Picture
paint_block :: Block -> Picture
paint_block b = Color (block_color b) (Polygon [(x,y),(x+bLOCK_SIZE,y),(x+bLOCK_SIZE,y-bLOCK_SIZE),(x,y-bLOCK_SIZE)])
	where
		y = bLOCK_SIZE * (snd . block_location) b
		x = bLOCK_SIZE * (fst . block_location) b

extent_block :: Block -> Extent
extent_block b = let (x,y) = block_location b in makeExtent (round y) (round y-round bLOCK_SIZE) (round x) (round x- round bLOCK_SIZE)

-- Makes a Tetromino from just TetroType t in the Tetronimo spawn location
mk_tetromino :: TetroType -> Tetromino
mk_tetromino t
	| t == L = Tetromino L (gRID!(x,y)) [Block (gRID!(x-1,y)) orange, Block (gRID!(x,y)) orange, Block (gRID!(x+1,y)) orange, Block (gRID!(x+1,y+1)) orange]
	| t == J = Tetromino J (gRID!(x,y)) [Block (gRID!(x-1,y+1)) blue, Block (gRID!(x-1,y)) blue, Block (gRID!(x,y)) blue, Block (gRID!(x+1,y)) blue]
	| t == I = Tetromino I (gRID!(x,y)) [Block (gRID!(x-2,y)) cyan, Block (gRID(x-1,y)) cyan, Block (gRID(x,y)) cyan, Block (gRID(x+1,y)) cyan]
	| t == O = Tetromino O (gRID!(x,y)) [Block (gRID!(x,y)) yellow, Block (gRID(x,y+1)) yellow, Block (gRID(x+1,y+1)) yellow, Block (gRID(x+1,y)) yellow]
	| t == S = Tetromino S (gRID!(x,y)) [Block (gRID!(x-1,y)) green, Block (gRID(x,y)) green, Block (gRID(x,y+1)) green, Block (gRID(x+1,y+1)) green]
	| t == Z = Tetromino Z (gRID!(x,y)) [Block (gRID!(x-1,y+1)) red, Block (gRID(x,y+1)) red, Block (gRID(x,y)) red, Block (gRID(x+1,y)) red]
	| t == T = Tetromino T (gRID!(x,y)) [Block (gRID!(x-1,y)) violet, Block (gRID(x,y)) violet, Block (gRID(x,y+1)) violet, Block (gRID(x+1,y)) violet]
	where
		x = 4
		y = 20
	
-- Translates teromino t to point (x,y)
translate_tetromino :: Tetromino -> Point -> Tetromino
translate_tetromino b (x,y) = Tetromino (tetromino_type b) (x,y) (blocks b)  

-- Rotates Tetromino t -pi/2
rotate_tetromino :: Tetromino -> Tetromino
rotate_tetromino t 
	| tetromino_type t == O = t
	| otherwise = Tetromino (tetromino_type t) (tetromino_location t) 
			[Block (-(snd . block_location) bl,(fst . block_location) bl) (block_color bl) | bl <- blocks t]

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
next_frame _ w
	| let t = active_tetr w in any (\e -> ((\(n,s,e,w) -> s) . takeExtent) e < -290) (extent_tetromino (active_tetr w)) = let t = active_tetr w in World (new_tetr' w) ((map (normalize_block t) (blocks t)) ++ inactive_blocks w)
	| otherwise = World (new_tetr w) (inactive_blocks w)
	where
		new_tetr' w = mk_tetromino S
		new_tetr w = let t = active_tetr w in translate_tetromino t ((fst . tetromino_location) t,(snd . tetromino_location) t - bLOCK_SIZE)
		--new_tetr w = rotate_tetromino (active_tetr w)

get_input :: Event -> World -> World
get_input (EventKey (SpecialKey KeySpace) Down _ _) w = World (rotate_tetromino (active_tetr w)) (inactive_blocks w)
get_input (EventKey (Char 'a') Down _ _) w = let t = active_tetr w in World (translate_tetromino (active_tetr w) ((fst . tetromino_location) t - bLOCK_SIZE,(snd . tetromino_location) t)) (inactive_blocks w)
get_input (EventKey (Char 's') Down _ _) w = let t = active_tetr w in World (translate_tetromino (active_tetr w) ((fst . tetromino_location) t,(snd . tetromino_location) t - bLOCK_SIZE)) (inactive_blocks w)
get_input (EventKey (Char 'd') Down _ _) w = let t = active_tetr w in World (translate_tetromino (active_tetr w) ((fst . tetromino_location) t + bLOCK_SIZE,(snd . tetromino_location) t)) (inactive_blocks w)
get_input _ w = w
