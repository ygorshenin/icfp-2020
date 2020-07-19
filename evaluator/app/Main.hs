{-# OPTIONS_GHC -O2 -optc-O2 #-}
module Main where

import Control.Monad
import Data.Char
import Data.List
import System.Environment
import System.IO
import Modem
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
          | x `Prelude.mod` y == 0 || x > 0 = x `div` y
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
              deriving (Eq, Show)

type Library = Map.Map String Entity

type Bounds = (Integer, Integer, Integer, Integer)
type Point = (Integer, Integer)

data RunResult = RunResult { flag :: Entity, state :: Entity, points :: [Point] }
                 deriving (Eq, Show)

entityFromPoint :: Point -> Entity
entityFromPoint (x, y) = Ap (Ap Cons (Number x)) (Number y)

-- Performs a single simplification step
simplifyStep :: Library -> Entity -> Entity
simplifyStep lib (Ap I x) = x
simplifyStep lib (Ap (Ap (Ap S x) y) z) = Ap xz yz
              where z' = simplify lib z
                    xz = Ap x z'
                    yz = Ap y z'
simplifyStep lib (Ap (Ap T x) _) = x
simplifyStep lib (Ap (Ap F _) y) = y
simplifyStep lib (Ap (Ap (Ap Cons x) y) f) = Ap (Ap f x) y
simplifyStep lib (Ap (Ap (Ap B x) y) z) = Ap x (Ap y z)
simplifyStep lib (Ap (Ap (Ap C x) y) z) = Ap (Ap x z) y
simplifyStep lib (Ap Nil _) = T
simplifyStep lib (Ap (Ap Add (Number x)) (Number y)) = Number (x + y)
simplifyStep lib (Ap (Ap Add x) y) = Ap (Ap Add x') y'
    where x' = simplifyStep lib x
          y' = simplifyStep lib y
simplifyStep lib (Ap (Ap Mul (Number x)) (Number y)) = Number (x * y)
simplifyStep lib (Ap (Ap Mul x) y) = Ap (Ap Mul x') y'
    where x' = simplifyStep lib x
          y' = simplifyStep lib y
simplifyStep lib (Ap Neg (Number x)) = Number (-x)
simplifyStep lib (Ap Neg x) = Ap Neg (simplifyStep lib x)
simplifyStep lib (Ap (Ap Eq (Number x)) (Number y)) = if x == y then T else F
simplifyStep lib (Ap (Ap Eq x) y) = Ap (Ap Eq x') y'
    where x' = simplifyStep lib x
          y' = simplifyStep lib y
simplifyStep lib (Ap (Ap Lt (Number x)) (Number y)) = if x < y then T else F
simplifyStep lib (Ap (Ap Lt x) y) = Ap (Ap Lt x') y'
    where x' = simplifyStep lib x
          y' = simplifyStep lib y
simplifyStep lib (Ap IsNil x) = case (simplifyStep lib x) of
                              Nil -> T
                              (Ap (Ap Cons _) _) -> F
                              y -> Ap IsNil y
simplifyStep lib (Ap Car x) = case (simplifyStep lib x) of
                            (Ap (Ap Cons x) _) -> x
                            y -> Ap y T
simplifyStep lib (Ap Cdr x) = case (simplifyStep lib x) of
                            (Ap (Ap Cons _) y) -> y
                            y -> Ap y F
simplifyStep lib (Ap (Ap Div (Number x)) (Number y)) = Number $ divTZ x y
simplifyStep lib (Ap (Ap Div x) y) = Ap (Ap Div x') y'
    where x' = simplifyStep lib x
          y' = simplifyStep lib y
simplifyStep lib (Ref name) = lib Map.! name
simplifyStep lib (Ap (Ref name) x) = Ap f x
    where f = lib Map.! name
simplifyStep lib (Ap (Ap (Ref name) x) y) = Ap (Ap f x) y
    where f = lib Map.! name
simplifyStep lib (Ap (Ap (Ap (Ref name) x) y) z) = Ap (Ap (Ap f x) y) z
    where f = lib Map.! name
simplifyStep lib (Ap f x) = Ap f' x
    where f' = simplifyStep lib f
simplifyStep _ x = x

-- Performs as many simplifications as possible
simplify :: Library -> Entity -> Entity
simplify lib e | e == e' = e
               | otherwise = simplify lib e'
    where e' = simplifyStep lib e

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

readLibrary :: String -> IO Library
readLibrary path = do
  lines <- liftM lines $ readFile path
  return . Map.fromList $ map makeASTS lines

data ParsedEntity = PENumber Integer
                  | PECons ParsedEntity ParsedEntity
                  | PENil
                    deriving (Show, Eq)

parseEntities :: Library -> Entity -> ParsedEntity
parseEntities lib e = case e' of
                        Number n -> PENumber n
                        (Ap (Ap Cons x) y) -> PECons (parseEntities lib x) (parseEntities lib y)
                        Nil -> PENil
                        e'' -> error $ "Unsupported entity: " ++ (show e'')
    where e' = simplify lib e

extractPoints :: ParsedEntity -> [Point]
extractPoints PENil = []
extractPoints (PECons (PENumber u) (PENumber v)) = [(u, v)]
extractPoints (PECons x y) = (extractPoints x) ++ (extractPoints y)

pointToPixel :: Bounds -> [Point] -> Point -> Char
pointToPixel (minWidth, minHeight, maxWidth, maxHeight) points (x, y)
    | x == (maxWidth + 1) = '\n'
    | (x == minWidth || x == maxWidth) && (y == minHeight || y == maxHeight) = '+'
    | x == minWidth || x == maxWidth = '|'
    | y == minHeight || y == maxHeight = '-'
    | (x, y) `elem` points = '#'
    | otherwise = ' '

draw :: [Point] -> String
draw points = [pointToPixel bounds points (x, y) |
               y <- [minHeight .. maxHeight],
               x <- [minWidth .. (maxWidth + 1)]]
    where xs = map fst points
          ys = map snd points

          minWidth = minimum xs - 2
          maxWidth = maximum xs + 2

          minHeight = minimum ys - 2
          maxHeight = maximum ys + 2

          bounds = (minWidth, minHeight, maxWidth, maxHeight)

suggestClicks :: Library -> RunResult -> [Point]
suggestClicks lib result = [point |
                            point <- points result,
                            let result' = runGalaxy lib (state result) (entityFromPoint point),
                            result' /= result]

runGalaxy :: Library -> Entity -> Entity -> RunResult
runGalaxy lib state point = RunResult flag' state' points'
    where galaxy = lib Map.! "galaxy"
          result = simplify lib $ Ap (Ap galaxy state) point
          flag' = simplify lib $ Ap Car result
          state' = simplify lib $ Ap Car (Ap Cdr result)
          data' = simplify lib $ Ap Cdr (Ap Cdr result)
          points' = extractPoints $ parseEntities lib data'

readPoint :: IO Point
readPoint = do
  putStr "Input points: "
  hFlush stdout
  points <- liftM (map read . words) getLine
  if length points /= 2
  then do
    putStrLn "Expected two points!"
    readPoint
  else return (points !! 0, points !! 1)

go :: Library -> Entity -> Entity -> IO ()
go lib state point = do
    let result@(RunResult flag' state' points') = runGalaxy lib state point
        suggest = head $ suggestClicks lib result
    putStrLn $ "Flag: " ++ (show flag')
    putStrLn $ "Points: " ++ (show points')
    putStrLn $ draw points'
    putStrLn $ "State: " ++ (show state)
    putStrLn $ "Suggested points: " ++ (show suggest)

    point <- readPoint
    go lib state' $ entityFromPoint point

instance Modem ParsedEntity where
  mod (PENumber a) = Modem.mod a
  mod (PECons x y) = "11" ++ (Modem.mod x) ++ (Modem.mod y)
  mod PENil        = "00"

  demod s = go u v
            where (u, v) = splitAt 2 s
                  go "00" v = (PENil, v)
                  go "11" v = ((PECons a b), z)
                  go _    v = (PENumber x, rest)
                  (a, w) = demod v
                  (b, z) = demod w
                  (x, rest) = demod s :: (Integer, String)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ fail "Expected path-to-galaxy.txt as an argument"
  lib <- readLibrary $ head args

  let state = Nil
      point = entityFromPoint (0, 0)
  go lib state point
