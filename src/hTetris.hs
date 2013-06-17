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



-- Generates the next game frame
next_frame :: Float -> World -> World
next_frame _ w = w

-- Handles Input
handle_input :: Event -> World -> World
handle_input (EventKey (Char 'w') Down _ _) w = World (attempt_rotate (active_tetromino w) w) (game_blocks w) (upcoming_tetrominos w)
handle_input (EventKey (Char 's') Down _ _) w = World (attempt_translate (active_tetromino w) ShiftDown w) (game_blocks w) (upcoming_tetrominos w)
handle_input (EventKey (Char 'a') Down _ _) w = World (attempt_translate (active_tetromino w) ShiftLeft w) (game_blocks w) (upcoming_tetrominos w)
handle_input (EventKey (Char 'd') Down _ _) w = World (attempt_translate (active_tetromino w) ShiftRight w) (game_blocks w) (upcoming_tetrominos w)

-- Handles non mapped inputs
handle_input _ w = w