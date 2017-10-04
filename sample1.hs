--Oct 03, 2017

--REPL

factorial :: Int -> Int
factorial 0 =1
factorial n = n * factorial(n-1)


-- Chapter 9
--type Parser = String -> Tree
--type Parser = String -> [(Tree, String)]
type Parser a = String -> [(a, String)]

return'  :: a -> Parser a
return' v inp = [(v, inp)]

failure :: Parser a
failure inp = []

item :: Parser Char
item inp = case inp of
    [] -> []
    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

-- >>= read as then
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
(p >>= f) inp = case parse p inp of
    [] -> []
    [(v, out)] -> parse(f v) out

(+++) :: Parser a -> Parser a -> Parser a
(p +++ q) inp = case parse p inp of
    [] -> parse q inp
    [(v, out)] -> [(v, out)]