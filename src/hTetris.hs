module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans
import Data.Array
import Tetromino
import Tetromino.Block

data World = World { active_tetromino :: Tetromino
					,game_blocks :: [Block]
					} deriving (Show)


new_world :: World
new_world = World (mk_tetromino L sPAWN) []

sPAWN :: Coord
sPAWN = (4,20)

-- Generates a new Tetromino from a string and returns the rotated string
next_tetromino :: String -> (Tetromino,String)
next_tetromino (n:ns)
	| n == 'L' = (mk_tetromino L sPAWN,ns ++ [n])
	| n == 'J' = (mk_tetromino J sPAWN,ns ++ [n])
	| n == 'I' = (mk_tetromino I sPAWN,ns ++ [n])
	| n == 'O' = (mk_tetromino O sPAWN,ns ++ [n])
	| n == 'S' = (mk_tetromino S sPAWN,ns ++ [n])
	| n == 'Z' = (mk_tetromino Z sPAWN,ns ++ [n])
	| n == 'T' = (mk_tetromino T sPAWN,ns ++ [n])
 
main :: IO ()
main = play (InWindow "hTetris" (300,600) (10,10)) white 1 new_world paint_world get_input next_frame

paint_world :: World -> Picture
paint_world w = Pictures $ (paint_tetromino (active_tetromino w)):[paint_block bl | bl <- game_blocks w]

next_frame :: Float -> World -> World
next_frame _ w = w -- World (new_tetr w) (game_blocks w)
	where
		--new_tetr w = rotate_tetromino (active_tetromino w)
		--new_tetr w = let t = active_tetromino w in translate_tetromino t ((fst . tetromino_location) t,(snd . tetromino_location) t - 1)
		--new_tetr w = mk_tetromino S (4,20)


get_input :: Event -> World -> World
-- Handles an input of 'w' and rotates the active tetromino
get_input (EventKey (Char 'w') Down _ _) w = let t = rotate_tetromino (active_tetromino w) in let bls = blocks t in if all ((\(x,y)-> x `elem` [0..9] && y `elem` [0..21]) . block_location) bls then
	World t (game_blocks w)
	else w
-- Handles an input of 'a' and translates the active tetromino to the left 1 bLOCK_SIZE
get_input (EventKey (Char 'a') Down _ _) w = let t = active_tetromino w in let x = (fst . tetromino_location) t - 1 in if x > 0 then
	World (translate_tetromino (active_tetromino w) (x,(snd . tetromino_location) t)) (game_blocks w)
	else w
-- Handles an input of 's' and translates the active tetromino down 1 bLOCK_SIZE
get_input (EventKey (Char 's') Down _ _) w = let t = active_tetromino w in let y = (snd . tetromino_location) t - 1 in if y >= 0 then 
	World (translate_tetromino (active_tetromino w) ((fst . tetromino_location) t,y)) (game_blocks w)
	else w
-- Handles an input of 'd' and translates the active tetromino to the right 1 bLOCK_SIZE
get_input (EventKey (Char 'd') Down _ _) w = let t = active_tetromino w in let x = (fst . tetromino_location) t + 1 in if x < 9 then
	World (translate_tetromino (active_tetromino w) (x,(snd . tetromino_location) t)) (game_blocks w)
	else w
-- Handles non mapped inputs
get_input _ w = w