{- EXAMPLE MODULE FOR TESTING -}
module Main where

import FFI
import Prelude

main :: Fay ()
main = alert "Hello, Sweet World!"

-- | Alert using window.alert.
alert :: String -> Fay ()
alert = ffi "window.alert(%1)"

