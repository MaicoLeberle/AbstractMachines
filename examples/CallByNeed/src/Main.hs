{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Monad.State.Lazy
import Text.Read

import AbstractMachines
import Types


main :: IO ()
main = do printMsg
          loop
  where
    printMsg :: IO ()
    printMsg = putStrLn $ concat [ "Milner abtract machine for the call-by-need"
                                 , " Linear Substituion Calculus lambda "
                                 , "calculus.\n"
                                 ]

    loop :: IO ()
    loop = getTerm >>= parseTerm >>= loopAux

    getTerm :: IO String
    getTerm = do putStrLn "Lambda term to evaluate:"
                 getLine

    parseTerm :: String -> IO CbNeedState
    parseTerm input =
        case parse input of
            Left err  -> do putStrLn (show err)
                            input <- getTerm
                            parseTerm input
            Right res -> pure $ initialState res

    loopAux :: CbNeedState -> IO ()
    loopAux st = do redFunc <- getRedFunction
                    reduce $ execState redFunc st
      where
        getRedFunction :: IO (State CbNeedState ())
        getRedFunction =
            do putStr "Number of reduction steps (ENTER: reduce to n.f.): "
               opt <- getLine
               if null opt then pure normalize
                           else case readMaybe @Integer opt of
                                    Nothing -> do putStr "Wrong option. "
                                                  getRedFunction
                                    Just n -> pure $ reduceNSteps n

        reduce :: CbNeedState -> IO ()
        reduce st | evalState isNormal st =
                        do putStrLn $ concat [ "Normal form:\n"
                                             , evalState decodeState st
                                             ]
                           loop
                  | otherwise =
                        do putStrLn $ concat [ "Normal form not yet reached. "
                                             , "Intermediate state:\n"
                                             , show st
                                             ]
                           loopAux st
