{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Ord

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

data Sol = Sol { solName :: String
               , solNum  :: Int
               , solLen  :: Int
               }
  deriving Show

main = do
  f <- readFile "counts.txt"
  let sols = readSols (lines f)
  multiMain (dias sols)
  
mkVis namePos n f sols
  = visualizeSols (map Just namePos ++ repeat Nothing) 
      (f $ sortBy (comparing solLen) (filter ((==n) . solNum) sols))
  
dias sols = [ ("1wo", mkVis [] 1 id sols)
            , ("1w",  mkVis [1,-1,1] 1 id sols)
            , ("2wo", mkVis [] 2 id sols)
            , ("2w",  mkVis [1,1,1] 2 id sols)
            , ("3wo", mkVis [] 3 init sols)
            , ("3w",  mkVis [1,1,1] 3 init sols)
            ]
      
visualizeSols :: [Maybe Double] -> [Sol] -> Diagram Cairo R2
visualizeSols namePos sols = 
  (mconcat . zipWith3 drawSol (cycle [1,-1]) namePos $ sols) 
  # centerX # pad 1.1
  where drawSol s namePos (Sol name _ len) = 
          ( vrule 5 
          # lw 0.5
          # lc blue 
          # lineCap LineCapRound
          # translateX (fromIntegral len)
          )
          <>
          ( text (show len) 
          # translateX (fromIntegral len) 
          # translateY (s * 5)
          # fontSize 3
          )
          <> 
          maybe mempty (\p ->
            ( alignedText (-p/2 + 1/2, 1/2) name
              # rotateBy (1/4)  
              # translateX (fromIntegral len)
              # translateY (p*10)
              # fontSize 4
            ))
            namePos

readSols :: [String] -> [Sol]      
readSols ss = evalState (execWriterT (mapM_ readOneLine ss)) ""
      
readOneLine :: String -> WriterT [Sol] (State String) ()
readOneLine (' ':s) = do
  name <- get
  let [num,len] = map read . words $ s
  tell [Sol name num len]
readOneLine name = put name
