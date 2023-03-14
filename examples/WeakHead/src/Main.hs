{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Monad.State.Lazy
import Text.Read

import AbstractMachines
import Types


main :: IO ()
main =
    putStrLn "Weak head abstract machine for the pure lambda calculus." >> aux
  where
    aux :: IO ()
    aux = do putStrLn ""
             input <- getTerm
             st <- parseTerm input
             loop st

    getTerm :: IO String
    getTerm = putStrLn "Lambda term to evaluate:" >> getLine

    parseTerm :: String -> IO WHState
    parseTerm input =
        case parse input of
            Left err  -> do putStrLn (show err)
                            input' <- getTerm
                            parseTerm input'
            Right res -> pure $ initialState res

    loop :: WHState -> IO ()
    loop st = do redFunc <- getRedFunction
                 reduce $ execState redFunc st
      where
        getRedFunction :: IO (State WHState ())
        getRedFunction =
            do putStr "Number of reduction steps (ENTER: reduce to n.f.): "
               opt <- getLine
               if null opt
                    then return normalize
                    else case readMaybe @Integer opt of
                            Nothing -> do putStr "Wrong option. "
                                          getRedFunction
                            Just n -> return $ reduceNSteps n

        reduce :: WHState -> IO ()
        reduce st
            | inNormalForm = do putStrLn ("Normal form:\n" ++ res)
                                aux
            | otherwise = do putStrLn $ concat [ "Normal form not yet reached. "
                                               , "Intermediate state: "
                                               , res
                                               ]
                             loop st
          where
            inNormalForm :: Bool
            inNormalForm = evalState isNormal st

            res :: String
            res = evalState decodeState st
