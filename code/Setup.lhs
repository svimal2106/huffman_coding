#!/usr/bin/runhaskell

cabal install --prefix=$HOME --user


> module Main (main) where

> import Distribution.Simple

> main :: IO ()
> main = defaultMain
