module Main where

import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Tuple
import Debug.Trace
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Modem
import System.Environment
import System.IO
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Game as Game
import qualified Lib

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

data RunResult = RunResult { flag :: Entity, state :: Entity, points :: [(Int, Point)] }
                 deriving (Eq, Show)

entityFromPoint :: Point -> Entity
entityFromPoint (x, y) = Ap (Ap Cons (Number x)) (Number y)

dirtyHack :: Entity -> Entity
dirtyHack T = Number 1
dirtyHack F = Number 0
dirtyHack x = x

-- Performs a single simplification step
simplifyStep :: Library -> Entity -> (Bool, Entity)
simplifyStep lib (Ap I x) = (True, simplify lib x)
simplifyStep lib (Ap (Ap (Ap S x) y) z) = (True, Ap xz yz)
              where z' = simplify lib z
                    xz = simplify lib (Ap x z')
                    yz = simplify lib (Ap y z')
simplifyStep lib (Ap (Ap T x) _) = (True, simplify lib x)
simplifyStep lib (Ap (Ap F _) y) = (True, simplify lib y)
simplifyStep lib (Ap (Ap (Ap Cons x) y) f) = (True, Ap (Ap f x) y)
simplifyStep lib (Ap (Ap (Ap B x) y) z) = (True, Ap x (Ap y z))
simplifyStep lib (Ap (Ap (Ap C x) y) z) = (True, Ap (Ap x z) y)
simplifyStep lib (Ap Nil _) = (True, T)
simplifyStep lib (Ap (Ap Add x) y) = (True, Number $ x' + y')
    where Number x' = dirtyHack $ simplify lib x
          Number y' = dirtyHack $ simplify lib y
simplifyStep lib (Ap (Ap Mul x) y) = (True, Number $ x' * y')
    where Number x' = dirtyHack $ simplify lib x
          Number y' = dirtyHack $ simplify lib y
simplifyStep lib (Ap Neg x) = (True, Number (-x'))
    where Number x' = dirtyHack $ simplify lib x
simplifyStep lib (Ap (Ap Eq x) y) = (True, if x' == y' then T else F)
    where Number x' = dirtyHack $ simplify lib x
          Number y' = dirtyHack $ simplify lib y
simplifyStep lib (Ap (Ap Lt x) y) = (True, if x' < y' then T else F)
    where Number x' = dirtyHack $ simplify lib x
          Number y' = dirtyHack $ simplify lib y
simplifyStep lib (Ap IsNil x) = case (simplify lib x) of
                              Nil -> (True, T)
                              (Ap (Ap Cons _) _) -> (True, F)
                              y -> (True, simplify lib $ Ap IsNil y)
simplifyStep lib (Ap Car x) = case (simplify lib x) of
                            (Ap (Ap Cons x) _) -> (True, simplify lib x)
                            y -> (True, simplify lib (Ap y T))
simplifyStep lib (Ap Cdr x) = case (simplify lib x) of
                            (Ap (Ap Cons _) y) -> (True, simplify lib y)
                            y -> (True, simplify lib (Ap y F))
simplifyStep lib (Ap (Ap Div x) y) = (True, Number $ divTZ x' y')
    where (Number x') = dirtyHack $ simplify lib x
          (Number y') = dirtyHack $ simplify lib y
simplifyStep lib (Ref name) = (True, simplify lib $ lib Map.! name)
simplifyStep lib (Ap (Ref name) x) = (True, simplify lib $ Ap f x)
    where f = lib Map.! name
simplifyStep lib (Ap (Ap (Ref name) x) y) = (True, simplify lib $ Ap (Ap f x) y)
    where f = lib Map.! name
simplifyStep lib (Ap (Ap (Ap (Ref name) x) y) z) = (True, simplify lib $ Ap (Ap (Ap f x) y) z)
    where f = lib Map.! name
simplifyStep lib (Ap f x) = (a, Ap f' x)
    where (a, f') = simplifyStep lib f
simplifyStep _ x = (False, x)

-- Performs as many simplifications as possible
simplify :: Library -> Entity -> Entity
simplify lib e = if changed then simplify lib e' else e
    where (changed, e') = simplifyStep lib e

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
                        (Ap (Ap Cons x) y) -> PECons (parseEntities lib x') (parseEntities lib y')
                            where x' = simplify lib x
                                  y' = simplify lib y
                        Nil -> PENil
                        e'' -> error $ "Unsupported entity: " ++ (show e'')
    where e' = simplify lib e

serializeEntities :: ParsedEntity -> Entity
serializeEntities (PENumber n) = Number n
serializeEntities (PECons x y) = Ap (Ap Cons (serializeEntities x)) (serializeEntities y)
serializeEntities PENil = Nil

extractPoints :: Int -> Int -> ParsedEntity -> [(Int, Point)]
extractPoints _ _ PENil = []
extractPoints itemIndex listIndex (PECons (PENumber u) (PENumber v)) = [(listIndex, (u, v))]
extractPoints itemIndex listIndex (PECons x y) = (extractPoints 0 itemIndex x) ++ (extractPoints (itemIndex + 1) listIndex y)

suggestClicks :: Library -> RunResult -> [Point]
suggestClicks lib result = [point |
                            point <- map snd $ points result,
                            let result' = runGalaxy lib (state result) (entityFromPoint point),
                            result' /= result]

send :: Library -> ParsedEntity -> IO ParsedEntity
send lib entity = do
  let input = Modem.mod entity
  output <- Lib.sendWithCurl input
  putStrLn $ "Input: " ++ (show input)
  putStrLn $ "Output: " ++ (show output)
  let (result, left) = demod output
  when (length left /= 0) . putStrLn $ "String left after parse: " ++ (show left)
  return result

runGalaxy :: Library -> Entity -> Entity -> RunResult
runGalaxy lib state point = unsafePerformIO $ do
    let galaxy = lib Map.! "galaxy"
        result = simplify lib $ Ap (Ap galaxy state) point
        flag' = simplify lib $ Ap Car result
        state' = simplify lib $ Ap Car (Ap Cdr result)
        data' = simplify lib $ Ap Cdr (Ap Cdr result)
        entities' = parseEntities lib data'
        points' = extractPoints 0 0 $ entities'
    case flag' of
      Number 0 -> return $ RunResult flag' state' points'
      _ -> do
        putStrLn $ "State: " ++ (show state')
        putStrLn $ "Flag: " ++ (show flag')
        entities'' <- send lib entities'
        putStrLn $ "Received: " ++ (show entities'')
        return $ runGalaxy lib state' (serializeEntities entities'')

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

data World = World Library RunResult Bool

makeSquarePath :: Float -> [(Float, Float)]
makeSquarePath size = [(0, 0), (size, 0), (size, size), (0, size)]

square :: Float -> Gloss.Color -> Gloss.Picture
square size color = Gloss.color color $ Gloss.polygon path
    where path = makeSquarePath size

squareWidth = 20

windowSize = (1280, 720)

makeViewport :: [Point] -> IO ViewPort
makeViewport points = do
  let (resx, resy) = windowSize
      xs = map fst points
      ys = map snd points

      xmin = (fromIntegral $ minimum xs - 1) * squareWidth
      xmax = (fromIntegral $ maximum xs + 2) * squareWidth

      ymin = (fromIntegral $ minimum ys - 1) * squareWidth
      ymax = (fromIntegral $ maximum ys + 2) * squareWidth

      scale = min ((fromIntegral resx) / (xmax - xmin)) ((fromIntegral resy) / (ymax - ymin))
  return $ ViewPort (-(xmin + xmax) / 2, -(ymin + ymax) / 2) 0 scale


both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

drawingFunc :: World -> Gloss.Picture
drawingFunc (World lib result suggestEnabled) = Gloss.scale 1 (-1) . applyViewPortToPicture viewPort $ Gloss.pictures (picturesMain ++ picturesSuggest)
    where viewPort = unsafePerformIO . makeViewport . map snd $ points result
          suggest = take 1 $ suggestClicks lib result

          innerPath = makeSquarePath squareWidth
          outerPath = makeSquarePath squareWidth

          squareMain = Gloss.polygon innerPath
          squareSuggest = Gloss.color Gloss.green $ Gloss.lineLoop outerPath

          depths = map fst $ points result
          minDepth = fromIntegral $ minimum depths
          maxDepth = fromIntegral $ maximum depths + 1
          depthWidth = maxDepth - minDepth

          combinedPoints = map head . groupBy ((==) `on` snd) . sortBy (compare `on` swap) $ points result
          picturesMain = [Gloss.translate (squareWidth * x' + 1) (squareWidth * y' + 1) $ Gloss.color color squareMain
                         | (d, p) <- combinedPoints
                         , let (x', y') = both fromIntegral p
                         , let color = Gloss.greyN $ (depthWidth - (fromIntegral d - minDepth)) / depthWidth]
          picturesSuggest = if suggestEnabled
                            then [Gloss.translate (squareWidth * x') (squareWidth * y') squareSuggest
                                 | p <- suggest
                                 , let (x', y') = both fromIntegral p]
                            else []

inputHandler :: Game.Event -> World -> World
inputHandler (Game.EventKey (Game.MouseButton Game.LeftButton) Game.Up _ (x, y)) (World lib result suggestEnabled) =
    World lib result' suggestEnabled
    where ps = map snd $ points result
          viewPort = unsafePerformIO $ makeViewport ps
          (x', y') = both (floor . (/ squareWidth)) $ invertViewPort viewPort (x, -y)
          result' = runGalaxy lib (state result) $ entityFromPoint (x', y')
inputHandler (Game.EventKey (Game.Char 's') Game.Up _ _) (World lib result suggestEnabled) =
    World lib result (not suggestEnabled)
inputHandler (Game.EventKey (Game.Char 'd') Game.Up _ _) w@(World _ result _) = unsafePerformIO $ do
    putStrLn $ "State: " ++ (show $ state result)
    return w
inputHandler _ world = world

updateFunc :: Float -> World -> World
updateFunc = flip const

skipIntro :: Library -> Entity -> [(Integer, Integer)] -> RunResult
skipIntro lib state [p] = runGalaxy lib state (entityFromPoint p)
skipIntro lib state (p:ps) = skipIntro lib state' ps
    where (RunResult flag' state' points') = runGalaxy lib state (entityFromPoint p)

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ fail "Expected path-to-galaxy.txt as an argument"
  lib <- readLibrary $ head args

  let state = Nil
      result = skipIntro lib state [(0, 0), (-1, -3), (-1, -3), (-1, -3), (-1, -3), (-3, -3), (0, -3), (0, 0), (8, 4), (2, -8), (3, 6), (0, -14), (-4, 10), (9, 8), (9, -3), (3, 10), (-4, 10), (13, 4)]

      world = World lib result False

  Gloss.play (Gloss.InWindow "Galaxy" windowSize (30, 30)) Gloss.black 0 world drawingFunc inputHandler updateFunc
