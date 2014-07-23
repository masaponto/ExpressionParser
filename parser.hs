{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Fay.Text as T
import Fay.FFI
import JQuery
import Prelude

import Data.Char


type Parser a = String -> [(a, String)]


parseInt :: String -> Int
parseInt = ffi "parseInt(%1)"


item :: Parser Char
item = \ inp -> case inp of
                  [] -> []
                  (x:xs) -> [(x,xs)]


failure :: Parser a
failure = \ _ -> []


parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp


return' :: a -> Parser a
return' v = \ inp -> [(v,inp)]


(>>-) :: Parser a -> (a -> Parser b) -> Parser b
p >>- f = \inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> parse (f v) out


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \ inp -> case parse p inp of
                     [] -> parse q inp
                     [(v, out)] -> [(v,out)]


sat :: (Char -> Bool) -> Parser Char
sat p = item >>- \x ->
        if p x then return' x else failure


digit :: Parser Char
digit = sat isDigit


char :: Char -> Parser Char
char x = sat (== x)


string :: String -> Parser String
string [] = return' []
string (x:xs) = char x >>- \ _ ->
                string xs >>- \_ ->
                return' (x:xs)


many :: Parser a -> Parser [a]
many p = many1 p +++ return'[]


many1 :: Parser a -> Parser [a]
many1 p = p >>- \v ->
          many p >>- \vs ->
          return' (v:vs)


nat :: Parser Int
nat = many1 digit >>- \xs ->
      return' (parseInt xs)


space :: Parser ()
space = many (sat isSpace) >>- \_ -> return' ()


--p :: Parser [Int]
--p = symbol "[" >>- \_ ->
--    natural >>- \n ->
--    many ( symbol "," >>- \_ ->
--           natural)  >>- \ns ->
--    symbol "]" >>- \_ ->
--    return' (n:ns)
--


token :: Parser a -> Parser a
token p = space >>- \_ ->
          p >>- \v ->
          space >>- \_ ->
          return' v


symbol :: String -> Parser String
symbol xs = token (string xs)


natural :: Parser Int
natural = token nat


expr :: Parser Int
expr = term >>- \t ->
       (
        (

         (
          symbol "+" >>- \_ ->
              expr >>- \e ->
                  return' (t + e)
         )

         +++ (
              symbol "-" >>- \_ ->
              expr >>- \e ->
                  return' (t - e)
             )

        )

        +++ return' t

       )


pow :: Parser Int
pow = factor >>- \f ->
      (
       (
        symbol "^" >>- \_ ->
        pow >>- \t ->
        return' (f^t)
       )

       +++ return' f
      )


term :: Parser Int
term = pow >>- \f ->
       (
        (

         (
          symbol "*" >>- \_ ->
          term >>- \t ->
          return' (f * t)
         )

         +++ (
              symbol "/" >>- \_ ->
              term >>- \t ->
              return' (f `div` t)

             )

        )

        +++ return' f

       )


factor :: Parser Int
factor = (

          symbol "(" >>- \_ ->
          (
           expr >>- \e ->
           symbol ")" >>- \_ ->
           return' e
          )

         )
         +++ intNum



intNum :: Parser Int
intNum = (
          symbol "-" >>- \_ ->
          natural >>- \x ->
          return' (-1 * x)
         ) +++ factrialP



factrialP :: Parser Int
factrialP = (
             natural >>- \x ->
             symbol "!" >>- \_ ->
             return' (factrial x)
            ) +++ natural


factrial :: Int -> Int
factrial x
    | x == 0 = 1
    | otherwise = x * factrial(x - 1)



eval :: String -> String
eval xs = case parse expr xs of
             [(n,[])] -> show n
             [(_,out)] -> "unused input " ++ out
             [] -> "invalid input"


main :: Fay ()
main = do
  input <- select $ T.pack "#inputText"
  output <- select $ T.pack "#outputText"

  let gen = do
         str <- getVal input
         setVal (T.pack $ eval $ T.unpack str) output
         return ()

  keyup (const gen) input
  onChange gen input

  return ()
