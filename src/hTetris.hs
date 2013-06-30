-- Main game logic and game logic functions.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import Control.Monad
import Control.Monad.Trans
import Data.List
import Data.Function
import Data.Maybe
import Data.Array
import Tetromino
import Tetromino.Block
import Control.Applicative
import Control.Monad
 
 -- Clears filled rows and increments score based on number of rows cleared.
attempt_clear :: World -> World
attempt_clear w = let 
  bls = game_blocks w
  fs = [(snd . block_location . head) r | 
        r <- groupBy (\a b -> (snd . block_location) a 
                              == (snd . block_location) b) $
             sortBy (compare `on` (snd . block_location)) bls,
        length r == 10] 
  lr = if null fs then 22 else minimum fs
  ls = length fs
  in
   World 
   (active_tetromino w) 
   ([if (snd . block_location) b > lr then  
       apply_gravity b w (length fs)
     else b
    | b <- filter (\b -> not $ (snd . block_location) b `elem` fs) bls])
   (upcoming_tetrominos w)
   (game_score w + if ls >= 4 then 2*ls else ls) (lock_timer w) (world_step w)

main :: IO ()
main = play (InWindow 
	"hTetris" 
	(300,600) 
	(10,10)) 
       white 60 nEW_WORLD 
       paint_world 
       handle_input 
       next_frame

-- Generates the next game frame.
next_frame :: Float -> World -> World
next_frame _ w
  | any (((==) 0) . snd) bls || any (==True) ((==) <$> bls <*> gbls) = 
    if lock_timer w <= 0 then
      attempt_clear $
      World (next_tetromino (upcoming_tetrominos w)) 
      (blocks t ++ game_blocks w) ((tail . upcoming_tetrominos) w) 
      (game_score w) lOCK_TIME (world_step w)
    else 
      World (active_tetromino w) gbs (upcoming_tetrominos w) (game_score w) 
      (lock_timer w - 1) (world_step w)
 | world_step w == 59 = 
      World (attempt_translate t ShiftDown w) 
      gbs
      (upcoming_tetrominos w) (game_score w) lOCK_TIME 0
 | otherwise = 
   World t gbs (upcoming_tetrominos w) (game_score w) 15 (world_step w + 1)
  where
    bls = map block_location (blocks t)
    gbls = map ((\(a,b) -> (a,b+1)) . block_location) (game_blocks w)
    t = active_tetromino w
    gbs = game_blocks w
    tbs = blocks t

-- Handles Input.
handle_input :: Event -> World -> World
handle_input (EventKey (Char 'w') Down _ _) w = 
  World (attempt_rotate (active_tetromino w) w) 
  (game_blocks w) (upcoming_tetrominos w) (game_score w) (lock_timer w)
  (world_step w)
handle_input (EventKey (Char 's') Down _ _) w =
  World (hard_drop (active_tetromino w) w) 
  (game_blocks w) (upcoming_tetrominos w) (game_score w) (lock_timer w)
  (world_step w)
handle_input (EventKey (Char 'a') Down _ _) w = 
  World (attempt_translate (active_tetromino w) ShiftLeft w) 
  (game_blocks w) (upcoming_tetrominos w) (game_score w) (lock_timer w)
  (world_step w)
handle_input (EventKey (Char 'd') Down _ _) w = 
  World (attempt_translate (active_tetromino w) ShiftRight w) 
  (game_blocks w) (upcoming_tetrominos w) (game_score w) (lock_timer w)
  (world_step w)

-- Handles non mapped inputs.
handle_input _ w = w