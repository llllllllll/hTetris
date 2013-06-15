module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans
import Data.Array
import Tetromino
import Tetromino.Block
import System.Random

data World = World { active_tetromino :: Tetromino
					,game_blocks :: [Block]
					,upcoming_tetrominos :: [TetroType]
					} deriving (Show)

-- A fresh game world with only 1 Tetromino at sPAWN and no game blocks
new_world :: World
new_world = World (mk_tetromino L sPAWN) [] random_types

random_types :: [TetroType]
random_types = map char_to_type $ randomRs (0::Int,5::Int) (mkStdGen 123)
	where
		char_to_type c
			| c == 0 = L
			| c == 1 = J
			| c == 2 = I
			| c == 3 = O
			| c == 4 = S
			| c == 5 = Z
			| c == 6 = T

-- Where to spawn new upcoming_tetrominos
sPAWN :: Coord
sPAWN = (4,20)

-- Pulls the next Tetromino out of the the list of random Types
next_tetromino :: [TetroType] -> (Tetromino,[TetroType])
next_tetromino (n:ns)
	| n == L = (mk_tetromino L sPAWN,ns)
	| n == J = (mk_tetromino J sPAWN,ns)
	| n == I = (mk_tetromino I sPAWN,ns)
	| n == O = (mk_tetromino O sPAWN,ns)
	| n == S = (mk_tetromino S sPAWN,ns)
	| n == Z = (mk_tetromino Z sPAWN,ns)
	| n == T = (mk_tetromino T sPAWN,ns)
 
main :: IO ()
main = play (InWindow 
	"hTetris" 
	(300,600) 
	(10,10)) 
	white 1 new_world paint_world handle_input next_frame

-- Converts a World into a Picture to be drawn to the screen
paint_world :: World -> Picture
paint_world w = Pictures $ (paint_tetromino 
		(active_tetromino w)):[paint_block bl | bl <- game_blocks w]

-- Generates the next game frame
next_frame :: Float -> World -> World
next_frame _ w = w -- World (new_tetr w) (game_blocks w)
	where
		--new_tetr w = rotate_tetromino (active_tetromino w)
		--new_tetr w = let t = active_tetromino w in translate_tetromino t ((fst . tetromino_location) t,(snd . tetromino_location) t - 1)
		--new_tetr w = mk_tetromino S (4,20)

-- Handles Input for 
handle_input :: Event -> World -> World

-- Handles an input of 'w' and rotates the active tetromino
handle_input (EventKey (Char 'w') Down _ _) w = let t = rotate_tetromino (active_tetromino w) in 
	let bls = blocks t in if all ((\(x,y)-> x `elem` [0..9] && y `elem` [0..21]) . block_location) bls then
	World t (game_blocks w) (upcoming_tetrominos w)
	else w

-- Handles an input of 's' and translates the active tetromino down 1 bLOCK_SIZE
handle_input (EventKey (Char 's') Down _ _) w = let t = active_tetromino w in let y = (snd . tetromino_location) t - 1 in 
	if y >= 0 then 
		if y == 0 then let r = upcoming_tetrominos w in World ((fst . next_tetromino) r) (blocks t ++ game_blocks w) (tail r)
		else World (translate_tetromino (active_tetromino w) ((fst . tetromino_location) t,y)) (game_blocks w) (upcoming_tetrominos w)
	else w

-- Handles an input of 'a' and translates the active tetromino to the left 1 bLOCK_SIZE
handle_input (EventKey (Char 'a') Down _ _) w = let t = active_tetromino w in let x = (fst . tetromino_location) t - 1 in if x > 0 then
	World (translate_tetromino (active_tetromino w) (x,(snd . tetromino_location) t)) (game_blocks w) (upcoming_tetrominos w)
	else w

-- Handles an input of 'd' and translates the active tetromino to the right 1 bLOCK_SIZE
handle_input (EventKey (Char 'd') Down _ _) w = let t = active_tetromino w in let x = (fst . tetromino_location) t + 1 in if x < 9 then
	World (translate_tetromino (active_tetromino w) (x,(snd . tetromino_location) t)) (game_blocks w) (upcoming_tetrominos w)
	else w

-- Handles non mapped inputs
handle_input _ w = w