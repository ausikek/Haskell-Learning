module Polimorphism where

    ziper (a:as) (b:bs) = (a, b):(ziper as bs)