-- |
-- Module      :  Main
-- Copyright   :  Mauro Taraborelli 2012
-- License     :  BSD3
--
-- Maintainer  :  maurotaraborelli@gmail.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Computes hashes traversing recursively through a directory structure.
-- Uses a list of known hashes to audit a set of files.

module Main where

import qualified HaskDeep.Command.Main as Command

main :: IO ()
main = Command.main