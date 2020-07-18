{-# OPTIONS_GHC -O2 -optc-O2 #-}
module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO
import qualified Data.Map as Map

-- Returns true if string represents a possibly negative number.
isInteger :: String -> Bool
isInteger "" = False
isInteger ('-':ds) = all isDigit ds
isInteger ds = all isDigit ds

-- Returns true if string represents a variable name.
isRef :: String -> Bool
isRef "galaxy" = True
isRef (':':_) = True
isRef _ = False

-- Integral division towards zero
divTZ :: Integer -> Integer -> Integer
divTZ x y | y == 0 = error "Division by zero"
          | y < 0 = divTZ (-x) (-y)
          | x `mod` y == 0 || x > 0 = x `div` y
          | otherwise = x `div` y + 1

data Entity = Add
            | Ap Entity Entity
            | B
            | C
            | Car
            | Cdr
            | Cons
            | Div
            | Eq
            | F
            | I
            | IsNil
            | Lt
            | Mul
            | Neg
            | Nil
            | Number Integer
            | S
            | T
            | Ref String
              deriving (Show, Eq)

type Context = Map.Map String Entity

-- Performs a single simplification step
simplifyStep :: Context -> Entity -> Entity
simplifyStep ctx (Ap I x) = x
simplifyStep ctx (Ap (Ap (Ap S x) y) z) = Ap xz yz
              where xz = Ap x z
                    yz = Ap y z
simplifyStep ctx (Ap (Ap T x) _) = x
simplifyStep ctx (Ap (Ap F _) y) = y
simplifyStep ctx (Ap (Ap (Ap Cons x) y) f) = Ap (Ap f x) y
simplifyStep ctx (Ap (Ap (Ap B x) y) z) = Ap x (Ap y z)
simplifyStep ctx (Ap (Ap (Ap C x) y) z) = Ap (Ap x z) y
simplifyStep ctx (Ap Nil _) = T
simplifyStep ctx (Ap (Ap Add (Number x)) (Number y)) = Number (x + y)
simplifyStep ctx (Ap (Ap Add x) y) = Ap (Ap Add x') y'
    where x' = simplifyStep ctx x
          y' = simplifyStep ctx y
simplifyStep ctx (Ap (Ap Mul (Number x)) (Number y)) = Number (x * y)
simplifyStep ctx (Ap (Ap Mul x) y) = Ap (Ap Mul x') y'
    where x' = simplifyStep ctx x
          y' = simplifyStep ctx y
simplifyStep ctx (Ap Neg (Number x)) = Number (-x)
simplifyStep ctx (Ap Neg x) = Ap Neg (simplifyStep ctx x)
simplifyStep ctx (Ap (Ap Eq (Number x)) (Number y)) = if x == y then T else F
simplifyStep ctx (Ap (Ap Eq x) y) = Ap (Ap Eq x') y'
    where x' = simplifyStep ctx x
          y' = simplifyStep ctx y
simplifyStep ctx (Ap (Ap Lt (Number x)) (Number y)) = if x < y then T else F
simplifyStep ctx (Ap (Ap Lt x) y) = Ap (Ap Lt x') y'
    where x' = simplifyStep ctx x
          y' = simplifyStep ctx y
simplifyStep ctx (Ap IsNil x) = case (simplifyStep ctx x) of
                              Nil -> T
                              (Ap (Ap Cons _) _) -> F
                              y -> Ap IsNil y
simplifyStep ctx (Ap Car x) = case (simplifyStep ctx x) of
                            (Ap (Ap Cons x) _) -> x
                            y -> Ap y T
simplifyStep ctx (Ap Cdr x) = case (simplifyStep ctx x) of
                            (Ap (Ap Cons _) y) -> y
                            y -> Ap y F
simplifyStep ctx (Ap (Ap Div (Number x)) (Number y)) = Number $ divTZ x y
simplifyStep ctx (Ap (Ap Div x) y) = Ap (Ap Div x') y'
    where x' = simplifyStep ctx x
          y' = simplifyStep ctx y
simplifyStep ctx (Ref name) = ctx Map.! name
simplifyStep ctx (Ap (Ref name) x) = Ap f x
    where f = ctx Map.! name
simplifyStep ctx (Ap (Ap (Ref name) x) y) = Ap (Ap f x) y
    where f = ctx Map.! name
simplifyStep ctx (Ap (Ap (Ap (Ref name) x) y) z) = Ap (Ap (Ap f x) y) z
    where f = ctx Map.! name
simplifyStep ctx (Ap f x) = Ap f' x
    where f' = simplifyStep ctx f
simplifyStep _ x = x

-- Performs as many simplifications as possible
simplify :: Context -> Entity -> Entity
simplify ctx e | e == e' = e
               | otherwise = simplify ctx e'
    where e' = simplifyStep ctx e

parseSimpleEnity :: String -> Entity
parseSimpleEnity "add" = Add
parseSimpleEnity "b" = B
parseSimpleEnity "c" = C
parseSimpleEnity "car" = Car
parseSimpleEnity "cdr" = Cdr
parseSimpleEnity "cons" = Cons
parseSimpleEnity "div" = Div
parseSimpleEnity "eq" = Eq
parseSimpleEnity "f" = F
parseSimpleEnity "i" = I
parseSimpleEnity "isnil" = IsNil
parseSimpleEnity "lt" = Lt
parseSimpleEnity "mul" = Mul
parseSimpleEnity "neg" = Neg
parseSimpleEnity "nil" = Nil
parseSimpleEnity "s" = S
parseSimpleEnity "t" = T
parseSimpleEnity s = error $ "Unknown entity: " ++ s

makeAST :: [String] -> [Entity]
makeAST [] = []
makeAST ("ap":ss) = (Ap x y) : rs
    where (x:y:rs) = makeAST ss
makeAST (s:ss) | isInteger s = (Number (read s)) : (makeAST ss)
               | isRef s = (Ref s) : (makeAST ss)
               | otherwise = (parseSimpleEnity s) : (makeAST ss)

makeASTS :: String -> (String, Entity)
makeASTS line = (name, entity)
    where (name:"=":rhs) = words line
          [entity] = makeAST rhs

readContext :: String -> IO Context
readContext path = do
  lines <- liftM lines $ readFile path
  return . Map.fromList $ map makeASTS lines

data ParsedEntity = PENumber Integer
                  | PECons ParsedEntity ParsedEntity
                  | PENil
                    deriving (Show, Eq)

parseEntities :: Context -> Entity -> ParsedEntity
parseEntities ctx e = case e' of
                        Number n -> PENumber n
                        (Ap (Ap Cons x) y) -> PECons (parseEntities ctx x) (parseEntities ctx y)
                        Nil -> PENil
                        e'' -> error $ "Unsupported entity: " ++ (show e'')
    where e' = simplify ctx e

extractCoords :: ParsedEntity -> [(Integer, Integer)]
extractCoords PENil = []
extractCoords (PECons (PENumber u) (PENumber v)) = [(u, v)]
extractCoords (PECons x y) = (extractCoords x) ++ (extractCoords y)

maxWidth = 10
maxHeight = 10

coordToPixel :: [(Integer, Integer)] -> Integer -> Integer -> Char
coordToPixel coords x y | x == (maxWidth + 1) = '\n'
                        | (x == (-maxWidth) || x == maxWidth) && (y == (-maxHeight) || y == maxHeight) = '+'
                        | x == (-maxWidth) || x == maxWidth = '|'
                        | y == (-maxHeight) || y == maxHeight = '-'
                        | (x, y) `elem` coords = '#'
                        | otherwise = ' '

draw :: [(Integer, Integer)] -> String
draw coords = [coordToPixel coords x y | y <- [-maxHeight .. maxHeight], x <- [-maxWidth .. (maxWidth + 1)]]

runGalaxy :: Context -> Entity -> Entity -> (Entity, Entity, [(Integer, Integer)])
runGalaxy ctx state point = (flag', state', coords')
    where galaxy = ctx Map.! "galaxy"
          result = simplify ctx $ Ap (Ap galaxy Nil) point
          flag' = simplify ctx $ Ap Car result
          state' = simplify ctx $ Ap Car (Ap Cdr result)
          data' = simplify ctx $ Ap Cdr (Ap Cdr result)
          coords' = extractCoords $ parseEntities ctx data'

readCoords :: IO (Integer, Integer)
readCoords = do
  putStr "Input coords: "
  hFlush stdout
  coords <- liftM (map read . words) getLine
  if length coords /= 2
  then do
    putStrLn "Expected two coords!"
    readCoords
  else return (coords !! 0, coords !! 1)

go :: Context -> Entity -> Entity -> IO ()
go ctx state points = do
    let (flag', state', coords') = runGalaxy ctx state points
    putStrLn $ "Flag: " ++ (show flag')
    putStrLn $ "Coords: " ++ (show coords')
    putStrLn $ draw coords'
    putStrLn $ "State: " ++ show state
    (x, y) <- readCoords
    go ctx state' (Ap (Ap Cons (Number x)) (Number y))

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ fail "Expected path-to-galaxy.txt as an argument"
  ctx <- readContext $ head args

  let state = Nil
      points = (Ap (Ap Cons (Number 0)) (Number 0))
  go ctx state points
