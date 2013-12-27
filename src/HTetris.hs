-- |
-- Module      : HTetris
-- Copyright   : Joe Jevnik 2013
--
-- License     : GPL-2
-- Maintainer  : joejev@gmail.org
-- Stability   : stable
-- Portability : GHC
--
-- Main game logic.

import HTetris.Tetromino
import HTetris.Data

import Control.Applicative ((<$>),(<*>))
import Data.List           (groupBy,sortBy)
import Data.Function       (on)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

 -- | Clears filled rows and increments score based on number of rows cleared.
attemptClear :: World -> World
attemptClear w =
    let  bs = gameBlocks w
         fs = [(snd . blockLocation . head) r
                   | r <- groupBy ((==) `on` (snd . blockLocation))
                          $ sortBy (compare `on` (snd . blockLocation)) bs
              , length r == 10]
         lr = if null fs
                then 22
                else minimum fs
         ls = length fs
    in w { gameBlocks = [(if lr <= (snd . blockLocation) b
                            then applyGravity b w $ ls
                            else b)
                         | b <- filter (\b -> not $ ((snd . blockLocation) b)
                                              `elem` fs) bs]
         , gameScore = gameScore w + ls
         }

main :: IO ()
main = getRandomTypes >>= \ts ->
       play (InWindow
	     "hTetris"
	     (300,600)
	     (10,10))
       white
       60
       (newWorld ts)
       paintWorld
       handleInput
       nextFrame

-- | Generates the next game frame.
nextFrame :: Float -> World -> World
nextFrame _ w@(World { worldStep = 59 }) =
    w { activeTetromino = attemptTranslate (activeTetromino w) ShiftDown w
      , lockTimer       = lockTime
      , worldStep       = 0
      }
nextFrame _ w = let bs = map blockLocation $ blocks $ activeTetromino w
                    gs = map (((id,(+) 1) <%>) . blockLocation) $ gameBlocks w
                in case ( lockTimer w <= 0
                        , any (((==) 0) . snd) bs || or ((==) <$> bs <*> gs)) of
                       (True,True)  -> attemptClear' w
                       (False,True) -> w { lockTimer = lockTimer w - 1 }
                       (False,_)    -> w { lockTimer = lockTime
                                         , worldStep = worldStep w + 1
                                         }
  where
      attemptClear' w = let (t',us) = nextTetromino $ upcomingTetrominos w
                        in attemptClear
                           w { activeTetromino    = t'
                             , gameBlocks         = blocks (activeTetromino w)
                                                    ++ gameBlocks w
                             , upcomingTetrominos = us
                             , lockTimer          = lockTime
                             }


-- | Handles Input.
handleInput :: Event -> World -> World
handleInput (EventKey (Char 'w') Down _ _) w =
    w { activeTetromino = attemptRotate (activeTetromino w) w               }
handleInput (EventKey (Char 's') Down _ _) w =
    w { activeTetromino = hardDrop (activeTetromino w) w                    }
handleInput (EventKey (Char 'a') Down _ _) w =
    w { activeTetromino = attemptTranslate (activeTetromino w) ShiftLeft w  }
handleInput (EventKey (Char 'd') Down _ _) w =
    w { activeTetromino = attemptTranslate (activeTetromino w) ShiftRight w }

-- Handles non mapped inputs.
handleInput _ w = w
