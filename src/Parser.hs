module Parser
    ( Parser.parse
    , unparse
    , newVarName
    , parseAndPrettyPrint
    ) where

import Data.Set
import Data.Tree
import Text.ParserCombinators.Parsec as P

import Types


parse :: String -> Either ParseError Term
parse = P.parse parseAux "Failed to parse."
  where
    parseAux :: GenParser Char st Term
    parseAux = do
        parseRes <- parseTerm
        eof
        return parseRes

    parseTerm :: GenParser Char st Term
    parseTerm =
            (parseVar >>= return . Var)
        <|> (do
                char '('
                res <- parseApp <|> parseLambda
                char ')'
                return res
            )

    parseVar :: GenParser Char st String
    parseVar = do
        i <- getLetter
        r <- parseVarAux ""
        return $ i : r
      where
        parseVarAux :: String -> GenParser Char st String
        parseVarAux varNamePrefix =
                (getLetter >>= parseVarAux . (: varNamePrefix))
            <|> (getDigit >>= parseVarAux . (: varNamePrefix))
            <|> return (reverse varNamePrefix)

        getLetter :: GenParser Char st Char
        getLetter = oneOf alphabet

        getDigit :: GenParser Char st Char
        getDigit = oneOf digits

    parseLambda :: GenParser Char st Term
    parseLambda = do
        char 'L'
        vName <- parseVar
        char '.'
        body <- parseTerm
        return $ Lambda vName body

    parseApp :: GenParser Char st Term
    parseApp = do
        firstTerm <- parseTerm
        char ' '
        secondTerm <- parseTerm
        return $ App firstTerm secondTerm

unparse :: Term -> String
unparse (Lambda v t) = "(L" ++ v ++ "." ++ unparse t ++ ")"
unparse (App t1 t2) = "(" ++ unparse t1 ++ " " ++ unparse t2 ++ ")"
unparse (Var v) = v

newVarName :: Set Variable -> Variable
newVarName sv = breakOnSets $ Prelude.map (:[]) alphabet
  where
    breakOnSets :: [Variable] -> Variable
    breakOnSets currLayer = case breakOnSetsAux currLayer of
        Just res -> res
        Nothing  -> let nextLayer = do p <- currLayer
                                       s <- alphabet ++ digits
                                       return (p ++ [s])
                    in breakOnSets nextLayer

    breakOnSetsAux :: [Variable] -> Maybe Variable
    breakOnSetsAux [] = Nothing
    breakOnSetsAux (y:ys) | not $ y `member` sv = Just y
                          | otherwise = breakOnSetsAux ys

parseAndPrettyPrint :: String -> IO ()
parseAndPrettyPrint input = case Parser.parse input of
    Left err -> putStrLn $ show err
    Right t  -> putStrLn $ drawTree $ convertTerm t
  where
    convertTerm :: Term -> Tree String
    convertTerm (Var v) =
        Node { rootLabel = "VAR"
             , subForest = [ Node {rootLabel = v, subForest = [] } ]
             }

    convertTerm (Lambda v t) =
        Node { rootLabel = "L" ++ v ++ "."
             , subForest = [convertTerm t]
             }

    convertTerm (App t s) =
        Node { rootLabel = "APP"
             , subForest = [convertTerm t, convertTerm s]
             }


-- | Auxiliary values.
alphabet :: [Char]
alphabet = ['a'..'z']

digits :: [Char]
digits = ['0'..'9']
