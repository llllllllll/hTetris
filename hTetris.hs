module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans


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

data Block = Block { block_open :: Bool
					,block_location :: Point
					,block_color :: Color
				   } deriving (Show, Eq)

gRID_SIZE :: Float
gRID_SIZE = 10

paint_Tetromino :: Tetromino -> Picture
paint_Tetromino b = Pictures [paint_block bl | bl <- blocks b]

paint_block :: Block -> Picture
paint_block b = Color (block_color b) (Polygon [(x-s,y+s),(x+s,y+s),(x+s,y-s),(x-s,y-s)])
	where
		y = (fst . block_location) b
		x = (snd . block_location) b
		s = gRID_SIZE/2

mk_tetromino :: TetroType -> Tetromino
mk_tetromino t
	| t == L = Tetromino L (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*0) orange,Block False (gRID_SIZE*3,gRID_SIZE*1) orange,Block False (gRID_SIZE*4,gRID_SIZE*1) orange,Block False (gRID_SIZE*5,gRID_SIZE*1) orange]
	| t == J = Tetromino J (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*1) blue,Block False (gRID_SIZE*4,gRID_SIZE*1) blue,Block False (gRID_SIZE*5,gRID_SIZE*1) blue,Block False (gRID_SIZE*5,gRID_SIZE*0) blue]
	| t == I = Tetromino I (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*1) cyan,Block False (gRID_SIZE*4,gRID_SIZE*1) cyan,Block False (gRID_SIZE*5,gRID_SIZE*1) cyan,Block False (gRID_SIZE*6,gRID_SIZE*1) cyan]
	| t == O = Tetromino O (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*0) yellow,Block False (gRID_SIZE*3,gRID_SIZE*1) yellow,Block False (gRID_SIZE*4,gRID_SIZE*0) yellow,Block False (gRID_SIZE*4,gRID_SIZE*1) yellow]
	| t == S = Tetromino S (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*1) green,Block False (gRID_SIZE*4,gRID_SIZE*1) green,Block False (4,0) green,Block False (gRID_SIZE*5,gRID_SIZE*0) green]
	| t == Z = Tetromino Z (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*0) red,Block False (gRID_SIZE*4,gRID_SIZE*0) red,Block False (4,1) red,Block False (5,1) red]
	| t == T = Tetromino T (gRID_SIZE*4,gRID_SIZE*1) [Block False (gRID_SIZE*3,gRID_SIZE*1) violet,Block False (gRID_SIZE*4,gRID_SIZE*1) violet,Block False (5,1) violet,Block False (gRID_SIZE*4,gRID_SIZE*0) violet]

translate_tetromino :: Tetromino -> Point -> Tetromino
translate_tetromino b (x,y) = Tetromino (tetromino_type b) (x,y) (trans' (blocks b) (x,y))
	where
		trans' bs (x,y) = [Block False ((fst . block_location) b - x,(snd . block_location) b) (block_color b) | b <- bs]

rotate_tetromino :: Tetromino -> Tetromino
rotate_tetromino b = Tetromino (tetromino_type b) (tetromino_location b) 
		[Block False ((fst . tetromino_location) b + (snd . tetromino_location) b - (snd . block_location) v,
			(snd . tetromino_location) b -(fst . tetromino_location) b + (fst . block_location) v) (block_color v) | v <- blocks b]

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
main = play (InWindow "hTetris" (400,600) (10, 10)) white 1 new_world paint_world get_input next_frame

paint_world :: World -> Picture
paint_world w = Pictures $ (paint_Tetromino (active_tetr w)):[paint_block bl | bl <- inactive_blocks w]

next_frame :: Float -> World -> World
next_frame _ w = World (new_tetr w) (inactive_blocks w)
	where
		new_tetr w = active_tetr w
		--new_tetr w = translate_Tetromino (active_tetr w) ((fst . tetromino_location) (active_tetr w),(snd . tetromino_location) (active_tetr w))
		--new_tetr w = rotate_tetromino (active_tetr w)

get_input :: Event -> World -> World
get_input e w
	| e == EventKey (Key 'w') Down [] (0,0) =  World (new_tetr w) (inactive_blocks w)
	where 
		new_tetr w = rotate_tetromino (active_tetr w)