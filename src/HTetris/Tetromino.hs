-- |
-- Module      : HTetris.Tetromino
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Functions that act on or with 'Tetromino's.

module HTetris.Tetromino
	( lockTime
	, paintWorld
        , nextTetromino
	, paintTetromino
        , attemptSpawn
	, attemptRotate
	, attemptTranslate
        , hardDrop
        , applyGravity ) where

import HTetris.Data
import HTetris.Tetromino.Block

import Control.Applicative ((<$>),(<*>))
import Control.Arrow       ((***))
import Data.Tuple          (swap)
import Graphics.Gloss


-- | Pulls the next Tetromino out of the the list of random Types.
nextTetromino :: [TetroType] -> (Tetromino,[TetroType])
nextTetromino (L:ts) = (mkTetromino L spawnLoc,ts)
nextTetromino (J:ts) = (mkTetromino J spawnLoc,ts)
nextTetromino (I:ts) = (mkTetromino I spawnLoc,ts)
nextTetromino (O:ts) = (mkTetromino O spawnLoc,ts)
nextTetromino (S:ts) = (mkTetromino S spawnLoc,ts)
nextTetromino (Z:ts) = (mkTetromino Z spawnLoc,ts)
nextTetromino (T:ts) = (mkTetromino T spawnLoc,ts)

-- | Checks if all the locations 'Tetromino' t would like to occupy in 'World' w
-- are availabe (not already occupied).
locationsAvailable :: Tetromino -> World -> Bool
locationsAvailable t w = let bs  = blocks t
                             gbs = gameBlocks w
                         in not $ or ((==) <$> bs <*> gbs)

-- | Converts a 'World' into a 'Picture' to be drawn to the screen.
paintWorld :: World -> Picture
paintWorld w = let bg = Pictures $ [Line [(x,-170),(x,270)]
                                        | x <- [-100,-100 + blockSize..100]]
                        ++ [Line [(-100,y),(100,y)]
                                | y <- [270,270 - blockSize..(-170)]]
               in Pictures [Pictures
                            $ --paintScore w
                             paintTetromino (activeTetromino w)
                            : paintNextTetromino ((head . upcomingTetrominos) w)
                            : [paintBlock bl | bl <- gameBlocks w],bg]
   where
      paintScore w =  w -- Translate (-150) (-275) $ Scale 0.5 0.5 $
                     -- Text (show (gameScore w))
      paintNextTetromino ty =
          Pictures
          [ Translate 75 (-200) (paintTetromino (mkTetromino ty (5,5))) ]
          -- , Translate (-75) (-230) $ Scale 0.125 0.125 $ Text "Next Tetromino:"]


-- | Converts Tetromino t to a Picture.
paintTetromino :: Tetromino -> Picture
paintTetromino t = Pictures [paintBlock bl | bl <- blocks t]

-- | Attempts to mkTetromino ty sPAWN and if it is unable to do so restarts
-- the game. TODO: add high scores and start/stop screens
attemptSpawn :: World -> World
attemptSpawn w = let (t,us) = nextTetromino $ upcomingTetrominos w
                 in if locationsAvailable t w
                    then w { activeTetromino    = t
                           , gameBlocks         = blocks t ++ gameBlocks w
                           , upcomingTetrominos = us
                           }
                    else  newWorld $ upcomingTetrominos w

-- | If Tetromino t can be rotated in World w then it returns the rotated
-- Tetromino, otherwise it returns the same one.
attemptRotate :: Tetromino -> World -> Tetromino
attemptRotate t w = let t' = rotateTetromino $ activeTetromino w
                    in  if all (\p -> snd p >= 0
                                && fst p <= 9
                                && fst p >= 0) (map blockLocation $ blocks t')
                            && locationsAvailable t' w
                          then t'
	                  else activeTetromino w

-- | If Tetromino t can be translated by shift in World w then it returns the
-- shifted Tetromino, otherwise it returns the same one.
attemptTranslate :: Tetromino -> Shift -> World -> Tetromino
attemptTranslate t ShiftDown w  = let t' = translateTetromino
                                           (activeTetromino w) ShiftDown
                                  in if all (\p -> snd p >= 0)
                                         (map blockLocation $ blocks t')
                                         && locationsAvailable t' w
                                       then t'
		                       else activeTetromino w
attemptTranslate t ShiftLeft w  = let t' = translateTetromino
                                           (activeTetromino w) ShiftLeft
                                  in if all (\p -> fst p >= 0)
                                         (map blockLocation $ blocks t')
                                         && locationsAvailable t' w
                                       then t'
		                       else activeTetromino w
attemptTranslate t ShiftRight w = let t' = translateTetromino
                                           (activeTetromino w) ShiftRight
                                  in if all (\p -> fst p <= 9)
                                         (map blockLocation $ blocks t')
                                        && locationsAvailable t' w
                                      then t'
		                      else activeTetromino w

-- | Translates teromino t to point (x,y)
translateTetromino :: Tetromino -> Shift -> Tetromino
translateTetromino t s  = t { tetrominoLocation =
                                  (case s of
                                       ShiftDown  -> (id *** flip (-) 1)
                                       ShiftRight -> ((+) 1 *** id)
                                       ShiftLeft  -> (flip (-) 1 *** id))
                                  $ tetrominoLocation t
                            , blocks            = map (translateBlock s)
                                                  $ blocks t
                            }


-- | Rotates Tetromino t by -pi/2.
rotateTetromino :: Tetromino -> Tetromino
rotateTetromino t@(Tetromino { tetrominoType = O }) = t
rotateTetromino t = t { blocks = [ b { blockLocation =
                                          ( uncurry (+) (tetrominoLocation t)
                                            - (snd . blockLocation) b
                                          , uncurry (-) (swap
                                                         $ tetrominoLocation t)
                                            + (fst . blockLocation) b)
                                    }
                                   | b <- blocks t]
                      }

-- | Applys the Tetris game gravity to Block b that exists within World w.
applyGravity :: Block -> World -> Int ->  Block
applyGravity b _ 0 = b
applyGravity b w c = applyGravity b { blockLocation = (id *** flip (-) 1)
                                                      $ blockLocation b
                                    } w (c - 1)

-- | Checks if a Coord loc is occupied within a World w.
locationAvailable :: Coord -> World -> Bool
locationAvailable loc w = any ((/=) loc . blockLocation) $ gameBlocks w

-- | Drops a block as far as possible down -TODO
hardDrop :: Tetromino -> World -> Tetromino
hardDrop t w
    | attemptTranslate t ShiftDown w == t = t
    | otherwise = hardDrop (attemptTranslate t ShiftDown w) w
