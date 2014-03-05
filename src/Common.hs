module Common where

roundToDec n v = (fromInteger $ round $ v * (10^n)) / (10.0^^n)

cmpDouble precision a b = round (a * 10 ^ precision) == round (b * 10 ^ precision)