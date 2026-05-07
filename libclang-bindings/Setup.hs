module Main (main) where

import Distribution.Simple (autoconfUserHooks, defaultMainWithHooks)

main :: IO ()
main = defaultMainWithHooks autoconfUserHooks
