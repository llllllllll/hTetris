-- Joe Jevnik
-- Functions and a data type to represent the Tetromino.
module Tetromino
	( TetroType(..)
	, Tetromino(..)
	, World(..)
	, nEW_WORLD
	, sPAWN
        , lOCK_TIME
	, paint_world
	, random_types
        , next_tetromino
	, paint_tetromino
        , attempt_spawn
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
import System.IO.Unsafe

-- Represents the type of tetromino.
data TetroType = L | J | I | O | S | Z | T deriving (Show, Eq)

-- Represents a tetromino game peice.
data Tetromino = Tetromino { tetromino_type :: TetroType
			   , tetromino_location :: Coord
			   , blocks :: [Block] 
                           }

-- Equality is based only on the location.
instance Eq Tetromino where
    (==) a b = tetromino_location a == tetromino_location b 
               && tetromino_type a == tetromino_type b
    (/=) a b = not $ a == b

data World = World { active_tetromino    :: Tetromino -- Controlable Tetromino
		   , game_blocks         :: [Block] -- Inactive blocks
		   , upcoming_tetrominos :: [TetroType]
                   , game_score          :: Int
                   , lock_timer          :: Int -- Frames until a Tetromino locks
                   , world_step          :: Int -- Current "frame"
		   }

-- A fresh game world with only 1 Tetromino at sPAWN and no game blocks
nEW_WORLD :: World
nEW_WORLD = World 
            (mk_tetromino (head random_types) 
                          sPAWN) 
            [] 
            random_types 
            0 
            lOCK_TIME
            0

-- Delay in frames before a tetromino locks.
lOCK_TIME :: Int
lOCK_TIME = 30

-- Where to spawn new upcoming_tetrominos.
sPAWN :: Coord
sPAWN = (4,20)

-- Pulls the next Tetromino out of the the list of random Types.
next_tetromino :: [TetroType] -> Tetromino
next_tetromino ty
    | head ty == L = mk_tetromino L sPAWN
    | head ty == J = mk_tetromino J sPAWN
    | head ty == I = mk_tetromino I sPAWN
    | head ty == O = mk_tetromino O sPAWN
    | head ty == S = mk_tetromino S sPAWN
    | head ty == Z = mk_tetromino Z sPAWN
    | head ty == T = mk_tetromino T sPAWN
                     
locations_available :: Tetromino -> World -> Bool
locations_available t w = not $ any (==True) ((==) <$> bls <*> gbls) 
  where
      bls = blocks t
      gbls = game_blocks w

-- Converts a World into a Picture to be drawn to the screen
paint_world :: World -> Picture
paint_world w = Pictures [Pictures $ 
                          (paint_score w):
                          (paint_tetromino (active_tetromino w)):
                          (paint_next_tetromino $ (head . upcoming_tetrominos) w)
                          :[paint_block bl | bl <- game_blocks w],bg]
  where
      bg = Pictures $ concat 
           [[Line [(x,-170),(x,270)] | x <- [-100,-100+bLOCK_SIZE..100]],
            [Line [(-100,y),(100,y)] | y <- [270,270-bLOCK_SIZE..0-170]]]
      paint_score w = (Translate (-150) (-275) $ Scale 0.5 0.5 $
                       (Text (show (game_score w))))
      paint_next_tetromino ty = 
          Pictures
          [ Translate 75 (-200) (paint_tetromino (mk_tetromino ty (5,5)))
          , Translate (-75) (-230) $ Scale 0.125 0.125 $ Text "Next Tetromino:"]

random_types :: [TetroType]
random_types = map int_to_type $ randomRs (0::Int,6::Int) 
               (unsafePerformIO getStdGen)
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
mk_tetromino ty (x,y)
    | ty == L = Tetromino L (x,y) [ Block (x-1,y) orange
                                  , Block (x,y) orange
                                  , Block (x+1,y) orange
                                  , Block (x+1,y+1) orange 
                                  ]
    | ty == J = Tetromino J (x,y) [ Block (x-1,y+1) blue 
                                  , Block (x-1,y) blue 
                                  , Block (x,y) blue 
                                  , Block (x+1,y) blue 
                                  ]
    | ty == I = Tetromino I (x,y-1) [ Block (x-2,y) cyan
                                    , Block (x-1,y) cyan 
                                    , Block (x,y) cyan 
                                    , Block (x+1,y) cyan
                                    ]
    | ty == O = Tetromino O (x,y) [ Block (x,y) yellow
                                  , Block (x,y+1) yellow 
                                  , Block (x+1,y+1) yellow 
                                  , Block (x+1,y) yellow
                                  ]
    | ty == S = Tetromino S (x,y) [ Block (x-1,y) green
                                  , Block (x,y) green
                                  , Block (x,y+1) green
                                  , Block (x+1,y+1) green
                                  ]
    | ty == Z = Tetromino Z (x,y) [ Block (x-1,y+1) red 
                                  , Block (x,y+1) red 
                                  , Block (x,y) red 
                                  , Block (x+1,y) red
                                  ]
    | ty == T = Tetromino T (x,y) [ Block (x-1,y) violet
                                  , Block (x,y) violet
                                  , Block (x,y+1) violet
                                  , Block (x+1,y) violet
                                  ]

-- Converts Tetromino t to a Picture.
paint_tetromino :: Tetromino -> Picture
paint_tetromino t = Pictures [paint_block bl | bl <- blocks t]

-- Attempts to mk_tetromino ty sPAWN and if it is unable to do so restarts
-- the game. TODO: add high scores and start/stop screens
attempt_spawn :: World -> World
attempt_spawn w = if locations_available t w 
                    then World t (blocks t ++ game_blocks w) 
                             ((tail . upcoming_tetrominos) w) 
                             (game_score w) lOCK_TIME (world_step w)
                    else  nEW_WORLD
  where
      t = next_tetromino (upcoming_tetrominos w)

-- If Tetromino t can be rotated in World w then it returns the rotated
-- Tetromino, otherwise it returns the same one.
attempt_rotate :: Tetromino -> World -> Tetromino
attempt_rotate t w = if all (\p -> snd p >= 0
                             && fst p <= 9 
                             && fst p >= 0) (map (block_location) (blocks t'))
                     && locations_available t' w 
                       then t'
	               else active_tetromino w
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
                       && locations_available t' w 
                  then t'
		  else active_tetromino w
    | shift == ShiftLeft =
		if all (\p -> fst p >= 0) (map (block_location) (blocks t')) 
                       && locations_available t' w 
                  then t'
		  else active_tetromino w
    | shift == ShiftRight = 
		if all (\p -> fst p <= 9) (map (block_location) (blocks t')) 
                       && locations_available t' w 
                  then t'
		  else active_tetromino w
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
                                - (fst . tetromino_location) t 
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
hard_drop t w 
    | attempt_translate t ShiftDown w == t = t
    | otherwise = hard_drop (attempt_translate t ShiftDown w) w
                  
