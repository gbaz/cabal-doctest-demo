{-# LANGUAGE CPP #-}

module MyLib where
import MyDep
import MyOtherDep
import Network.Socket

#if defined(WORD_SIZE_IN_BITS)
type instance IntBaseType Int    = 'FixedIntTag  WORD_SIZE_IN_BITS
type instance IntBaseType Word   = 'FixedWordTag WORD_SIZE_IN_BITS
#else
#endif


-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> fib 5
-- 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)


-- |
-- >>> let x = 23
-- >>> x + 42
-- 65
someFunc :: IO ()
someFunc = putStrLn "someFunccc"


{- | >>> :{
let x = 1
    y = 2
  in x + y + works
:}
6
-}
works = 3