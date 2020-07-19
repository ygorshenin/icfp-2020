module Modem where

import Data.List

class Modem a where
    mod :: a -> String
    demod :: String -> (a, String)

toBinaryString :: Integer -> String
toBinaryString x
  | x < 0  = "error"
  | x == 0 = "0"
  | x > 0  = pad $ go x
             where go 0 = ""
                   go x = (go $ x `div` 2) ++ (bit $ x `Prelude.mod` 2)
                   bit 0 = "0"
                   bit 1 = "1"
                   pad s | (length s) `Prelude.mod` 4 == 0 = s
                         | otherwise                       = pad $ "0" ++ s

fromBinaryString :: String -> Integer
fromBinaryString = foldl' (\ x y -> 2*x + fromBit y) 0
                   where fromBit '0' = 0
                         fromBit '1' = 1

instance Modem Integer where
    mod 0 = "010"
    mod x = (modSign . signum $ x) ++ (modAbs . abs $ x)
            where modSign (-1) = "10"
                  modSign _    = "01"
                  modAbs x = (unary $ length s) ++ s
                  s = toBinaryString x
                  unary n
                      | n `Prelude.mod` 4 == 0 = (concat $ replicate (n `div` 4) "1") ++ "0"

    demod s = (demodPrefix, rest)
              where demodPrefix = (demodSign signPart) * (fromBinaryString absPart)
                    demodSign "10" = -1
                    demodSign "01" = 1
                    (signPart, u) = splitAt 2 s
                    (lenPart, v) = span (== '1') u
                    (absPart, rest) = splitAt (4 * (length lenPart) + 1) v
