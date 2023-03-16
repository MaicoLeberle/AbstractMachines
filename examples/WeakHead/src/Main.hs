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
    printMsg = putStrLn $ concat [ "Weak head abstract machine for the pure "
                                 , "lambda calculus.\n"
                                 ]

    loop :: IO ()
    loop = getTerm >>= parseTerm >>= loopAux

    getTerm :: IO String
    getTerm = do putStrLn "Lambda term to evaluate:"
                 getLine

    parseTerm :: String -> IO WHState
    parseTerm input =
        case parse input of
            Left err  -> do putStrLn (show err)
                            input' <- getTerm
                            parseTerm input'
            Right res -> pure $ initialState res

    loopAux :: WHState -> IO ()
    loopAux st = do redFunc <- getRedFunction
                    reduce $ execState redFunc st
      where
        getRedFunction :: IO (State WHState ())
        getRedFunction =
            do putStr "Number of reduction steps (ENTER: reduce to n.f.): "
               opt <- getLine
               if null opt then pure normalize
                           else case readMaybe @Integer opt of
                                    Nothing -> do putStr "Wrong option. "
                                                  getRedFunction
                                    Just n -> pure $ reduceNSteps n

        reduce :: WHState -> IO ()
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
