{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Fay.Text as T
import Fay.FFI
import JQuery
import Prelude
import Data.Char


type Parser a = String -> [(a, String)]


parseDouble :: String -> Double
parseDouble = ffi "parseFloat(%1)"


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


space :: Parser ()
space = many (sat isSpace) >>- \_ -> return' ()


token :: Parser a -> Parser a
token p = space >>- \_ ->
          p >>- \v ->
          space >>- \_ ->
          return' v


symbol :: String -> Parser String
symbol xs = token (string xs)


nat :: Parser Double
nat = many1 digit  >>- \xs->
       (
        ( symbol "." >>- \_ ->
          ( many1 digit ) >>- \ys ->
          return' $ parseDouble $ xs ++ "." ++ ys )

        +++ return' (parseDouble xs)

       )


decimal :: Parser Double
decimal = token nat


expr :: Parser Double
expr = term >>- \t ->
       (
        (

         (
          many (
                (
                 symbol "-" >>- \_  ->
                 term >>- \x ->
                 return' (-x)
                ) +++ ( symbol "+" >>- \_ -> term)
               ) >>- \ns ->
          return' (foldl (+) t ns)
         )

        ) +++ return' t

       )


term :: Parser Double
term = pow >>- \p ->
       (
        (

         (
          many (
                (
                 symbol "/" >>- \_  ->
                 pow >>- \x ->
                 return' (1/x)
                ) +++ ( symbol "*" >>- \_ -> pow )
               ) >>- \ns ->
          return' (foldl (*) p ns)
         )

        ) +++ return' p

       )

pow :: Parser Double
pow = factorialP >>- \f ->
      (
       (
        symbol "^" >>- \_ ->
        pow >>- \p ->
        return' (f ** p)
       ) +++ return' f
      )


factorialP :: Parser Double
factorialP =  factor >>- \f ->
             (
              symbol "!" >>- \_ ->
              -- return' $ factorial x
              return' (fixDouble $ gamma $ f + 1 )
            ) +++ return' f


-- isDicimal :: Double -> Bool
-- isDicimal x = (take 1 as) /= "0"
--     where as = tail $ dropWhile (/= '.') $ show x
--
--
-- factorial :: Double -> Double
-- factorial x
--     | isDicimal x = gamma (x + 1)
--     | otherwise = product [2..x]
--


factor :: Parser Double
factor = (

          symbol "(" >>- \_ ->
          (
           expr >>- \e ->
           symbol ")" >>- \_ ->
           return' e
          )

         ) +++ number


number :: Parser Double
number = (
          symbol "-" >>- \_ ->
          decimal >>- \x ->
          return' (-1 * x)
         ) +++ decimal


fixDouble :: Double -> Double
fixDouble x
    | isFixable x = ( parseDouble $ takeWhile (/= '.') $ show x ) + 1
    | otherwise = x


isFixable :: Double -> Bool
isFixable x = (take 2 as) == "99"
    where as = tail $ dropWhile (/= '.') $ show x


gamma :: Double -> Double
gamma x = sqrt(2.0*pi) * (x+mu)**(x-0.5) * e**(-(x + mu)) * (largeA x)

    where e = 2.71828182846
          mu = 4.5
          c0 =   1.000000000178
          c1 =  76.180091729406
          c2 = -86.505320327112
          c3 =  24.014098222230
          c4 =  -1.231739516140
          c5 =   0.001208580030
          c6 =  -0.000005363820
          largeA y = c0 + c1/y + c2/(y+1.0) + c3/(y+2.0) + c4/(y+3.0) + c5/(y+4.0) + c6/(y+5.0)


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
