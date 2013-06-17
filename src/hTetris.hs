module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans
import Data.Array
import Tetromino
import Tetromino.Block


-- Pulls the next Tetromino out of the the list of random Types
next_tetromino :: [TetroType] -> Tetromino
next_tetromino ty
	| head ty == L = mk_tetromino L sPAWN
	| head ty == J = mk_tetromino J sPAWN
	| head ty == I = mk_tetromino I sPAWN
	| head ty == O = mk_tetromino O sPAWN
	| head ty == S = mk_tetromino S sPAWN
	| head ty == Z = mk_tetromino Z sPAWN
	| head ty == T = mk_tetromino T sPAWN
 
main :: IO ()
main = play (InWindow 
	"hTetris" 
	(300,600) 
	(10,10)) 
	white 1 new_world paint_world handle_input next_frame



-- Generates the next game frame
next_frame :: Float -> World -> World
next_frame _ w = 
	if any (\(n,s,e,w) -> s == 0) (map takeExtent (tetromino_extent t)) && foldl (&&) True (zipWith (==) lgbs ltbs) then
			World (next_tetromino (upcoming_tetrominos w)) (tbs ++ game_blocks w) ((tail . upcoming_tetrominos) w)
		else
			w
	where
		t = active_tetromino w
		gbs = game_blocks w
		tbs = blocks t
		lgbs = map block_location (game_blocks w)
		ltbs = map block_location (blocks t)

-- Handles Input
handle_input :: Event -> World -> World
handle_input (EventKey (Char 'w') Down _ _) w = World (attempt_rotate (active_tetromino w) w) (game_blocks w) (upcoming_tetrominos w)
handle_input (EventKey (Char 's') Down _ _) w = World (attempt_translate (active_tetromino w) ShiftDown w) (game_blocks w) (upcoming_tetrominos w)
handle_input (EventKey (Char 'a') Down _ _) w = World (attempt_translate (active_tetromino w) ShiftLeft w) (game_blocks w) (upcoming_tetrominos w)
handle_input (EventKey (Char 'd') Down _ _) w = World (attempt_translate (active_tetromino w) ShiftRight w) (game_blocks w) (upcoming_tetrominos w)

-- Handles non mapped inputs
handle_input _ w = w