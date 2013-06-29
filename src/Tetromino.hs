-- Functions and a data type to represent the Tetromino.
module Tetromino
	( TetroType(..)
	, Tetromino(..)
	, World(..)
	, new_world
	, sPAWN
	, paint_world
	, random_types
	, mk_tetromino
	, paint_tetromino
	, tetromino_extent
	, attempt_rotate
	, attempt_translate
        , hard_drop
        , apply_gravity ) where

import Tetromino.Block
import Graphics.Gloss
import Graphics.Gloss.Data.Extent
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Control.Applicative
import Control.Monad

-- Represents the type of tetromino.
data TetroType = L | J | I | O | S | Z | T deriving (Show, Eq)

-- Represents a tetromino game peice.
data Tetromino = Tetromino {tetromino_type :: TetroType
				,tetromino_location :: Coord
				,blocks :: [Block] 
                                } deriving (Show, Eq)

data World = World { active_tetromino :: Tetromino
			,game_blocks :: [Block]
			,upcoming_tetrominos :: [TetroType]
                        ,game_score :: Int
			} deriving (Show)

-- A fresh game world with only 1 Tetromino at sPAWN and no game blocks
new_world :: World
new_world = World (mk_tetromino (head random_types) sPAWN) [] random_types 0

-- Where to spawn new upcoming_tetrominos
sPAWN :: Coord
sPAWN = (4,20)

locations_available :: Tetromino -> World -> Bool
locations_available t w = not $ any (==True) ((==) <$> bls <*> gbls) 
	where
		bls = map block_location (blocks t)
		gbls = map block_location (game_blocks w)

-- Converts a World into a Picture to be drawn to the screend
paint_world :: World -> Picture
paint_world w = Pictures [Pictures $ 
               (paint_score w):
               (paint_tetromino (active_tetromino w)):
               (paint_next_tetromino $ (head .  upcoming_tetrominos) w):
               [paint_block bl | bl <- game_blocks w],bg]
  where
    bg = Pictures $ concat 
         [[Line [(x,-170),(x,300)] | x <- [-100,-80..100]],
          [Line [(-100,y),(100,y)] | y <- [310,290..0-170]]]
    paint_score w = (Translate (-150) (-275) $ Scale 0.5 0.5 $
                     (Text (show (game_score w))))
    paint_next_tetromino ty = Pictures
      [Translate 75 (-200) (paint_tetromino (mk_tetromino ty (5,5))),
       Translate (-75) (-230) $ Scale 0.125 0.125 $ Text "Next Tetromino:"]

random_types :: [TetroType]
random_types = map int_to_type $ randomRs (0::Int,5::Int) (mkStdGen 123)
	where
		int_to_type c
			| c == 0 = L
			| c == 1 = J
			| c == 2 = I
			| c == 3 = O
			| c == 4 = S
			| c == 5 = Z
			| c == 6 = T

-- Makes a Tetromino from just TetroType t at Point (x,y).
mk_tetromino :: TetroType -> Coord -> Tetromino
mk_tetromino t (x,y)
	| t == L = Tetromino L (x,y) [Block (x-1,y) orange, 
                                      Block (x,y) orange, 
                                      Block (x+1,y) orange, 
                                      Block (x+1,y+1) orange]
	| t == J = Tetromino J (x,y) [Block (x-1,y+1) blue, 
                                      Block (x-1,y) blue, 
                                      Block (x,y) blue, 
                                      Block (x+1,y) blue]
        | t == I = Tetromino I (x,y-1) [Block (x-2,y) cyan, 
                                        Block (x-1,y) cyan, 
                                        Block (x,y) cyan, 
                                        Block (x+1,y) cyan]
	| t == O = Tetromino O (x,y) [Block (x,y) yellow, 
                                      Block (x,y+1) yellow, 
                                      Block (x+1,y+1) yellow, 
                                      Block (x+1,y) yellow]
	| t == S = Tetromino S (x,y) [Block (x-1,y) green, 
                                      Block (x,y) green, 
                                      Block (x,y+1) green, 
                                      Block (x+1,y+1) green]
	| t == Z = Tetromino Z (x,y) [Block (x-1,y+1) red, 
                                      Block (x,y+1) red, 
                                      Block (x,y) red, 
                                      Block (x+1,y) red]
	| t == T = Tetromino T (x,y) [Block (x-1,y) violet, 
                                      Block (x,y) violet, 
                                      Block (x,y+1) violet, 
                                      Block (x+1,y) violet]

-- Converts Tetromino t to a Picture.
paint_tetromino :: Tetromino -> Picture
paint_tetromino t = Pictures [paint_block bl | bl <- blocks t]

-- Gets the bounding boxes of Tetromino t.
tetromino_extent :: Tetromino -> [Extent]
tetromino_extent t = map block_extent (blocks t)

attempt_rotate :: Tetromino -> World -> Tetromino
attempt_rotate t w =
	if all (\p -> snd p >= 0 && 
                      fst p <= 9 && 
                      fst p >= 0) (map (block_location) (blocks t')) 
           && locations_available t' w then
		t'
	else
		active_tetromino w
	where
		t' = rotate_tetromino (active_tetromino w)
		gbs = game_blocks w
		tbs = blocks t'

-- If Tetromino t can be translated by shift in World w then it returns the 
-- shifted Tetromino, otherwise it returns the same one.
attempt_translate :: Tetromino -> Shift -> World -> Tetromino
attempt_translate t shift w
	| shift == ShiftDown = 
		if all (\p -> snd p >= 0) (map (block_location) (blocks t')) 
                   && locations_available t' w then
			t'
		else
			active_tetromino w
	| shift == ShiftLeft =
		if all (\p -> fst p >= 0) (map (block_location) (blocks t')) 
                   && locations_available t' w then
			t'
		else
			active_tetromino w
	| shift == ShiftRight = 
		if all (\p -> fst p <= 9) (map (block_location) (blocks t')) 
                   && locations_available t' w then
			t'
		else
			active_tetromino w
	where
		t' = translate_tetromino (active_tetromino w) shift
		gbs = game_blocks w
		tbs = blocks t'

-- Translates teromino t to point (x,y)
translate_tetromino :: Tetromino -> Shift -> Tetromino
translate_tetromino t shift
	| shift == ShiftDown = 
          Tetromino (tetromino_type t) 
          ((fst . tetromino_location) t,
           (snd . tetromino_location) t - 1) 
          (map (translate_block shift) (blocks t))
	| shift == ShiftLeft = 
            Tetromino (tetromino_type t) 
            ((fst . tetromino_location) t - 1,
             (snd . tetromino_location) t) 
            (map (translate_block shift) (blocks t))
	| shift == ShiftRight = 
              Tetromino (tetromino_type t) 
              ((fst . tetromino_location) t + 1,
               (snd . tetromino_location) t) 
              (map (translate_block shift) (blocks t))

-- Rotates Tetromino t by -pi/2.
rotate_tetromino :: Tetromino -> Tetromino
rotate_tetromino t 
	| tetromino_type t == O = t
	| otherwise = 
          Tetromino (tetromino_type t) 
          (tetromino_location t) 
           [Block ((fst . tetromino_location) t 
                   + (snd . tetromino_location) t 
                   - (snd . block_location) bl,
                   -(fst . tetromino_location) t 
                   + (snd . tetromino_location) t 
                   + (fst . block_location) bl) 
            (block_color bl) | bl <- blocks t]
          
-- Applys the Tetris game gravity to Block b that exists within World w.
apply_gravity :: Block -> World -> Int ->  Block
apply_gravity b w c  
  | c == 0 = b
  | otherwise = apply_gravity 
                (Block ((fst . block_location) b,
                        (snd . block_location) b - 1) (block_color b)) w (c-1)
-- Checks if a Coord loc is occupied within a World w.
location_available :: Coord -> World -> Bool
location_available loc w = 
  not $ any (\b -> block_location b == loc) (game_blocks w)

-- Drops a block as far as possible down -TODO
hard_drop :: Tetromino -> World -> Tetromino
hard_drop t w = attempt_translate t ShiftDown w
                  