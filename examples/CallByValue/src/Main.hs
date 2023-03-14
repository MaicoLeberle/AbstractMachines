{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Exception
import Control.Monad.State.Lazy
import Text.Read

import AbstractMachines
import Types


main :: IO ()
main = do
    putStrLn ("Leroy abtract machine for the right-to-left, call-by-value, pure"
                ++ " lambda calculus.")
    aux
  where
    aux :: IO ()
    aux = putStrLn "" >> getTerm >>= parseTerm >>= loop

    getTerm :: IO String
    getTerm = putStrLn "Lambda term to evaluate:" >> getLine

    parseTerm :: String -> IO CbVState
    parseTerm input =
        case parse input of
            Left err  -> putStrLn (show err) >> getTerm >>= parseTerm
            Right res -> pure $ initialState res

    loop :: CbVState -> IO ()
    loop st = getRedFunction >>= reduce . ($ st) . (execState)
      where
        getRedFunction :: IO (State CbVState ())
        getRedFunction = do
            putStr "Number of reduction steps (ENTER: reduce to n.f.): "
            opt <- getLine
            if null opt
                then return normalize
                else case readMaybe @Integer opt of
                        Nothing -> putStr "Wrong option. " >> getRedFunction
                        Just n -> return $ reduceNSteps n

        reduce :: CbVState -> IO ()
        reduce st
            | inNormalForm = do
                putStrLn ("Normal form:\n" ++ evalState decodeState st)
                aux
            | otherwise = do
                putStrLn
                    $ "Normal form not yet reached. Intermediate state:\n" ++ show st
                loop st
          where
            inNormalForm :: Bool
            inNormalForm = evalState isNormal st
