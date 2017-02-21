module Main (main) where

import Flow ((|>))
import System.Environment (getArgs, getProgName)
import Text.Printf (printf)

import MortgageCalc (calcMonthlyPayment, calcAmortization)

{- TODO
 - Prelude.read: no parse
 - exit code
 -}

main = getArgs >>= \args -> getProgName >>= \progName ->
  case args of
    [       loan, annualInterestRate, years ] ->
      let (l, i, n) = readArgs loan annualInterestRate years
      in calcMonthlyPayment l i n |> showMonthlyPayment n
    [ "-v", loan, annualInterestRate, years ] ->
      let (l, i, n) = readArgs loan annualInterestRate years
          p = calcMonthlyPayment l i n
          a = calcAmortization p l i n
      in showMonthlyPayment n p ++ showAmortization a
    otherwise                                 -> showUsage progName
  |> putStr

readArgs :: String -> String -> String -> (Float, Float, Int)
readArgs loan annualInterestRate years = (read loan, (read annualInterestRate) / 100, read years)

showMonthlyPayment :: Int -> Float -> String
showMonthlyPayment n p =
  printf "Your monthly payment is $%f, and the total payment is $%f.\n" p (p * 12 * fromIntegral n)

showAmortization :: [(Int, Float, Float, Float)] -> String
showAmortization a =
  printf "Detailed amortization:\n\
         \%7s%22s%22s%22s\n" "Month #" "Principal" "Interest" "Balance"
    ++ foldl showOneTuple "" a
  where
    showOneTuple :: String -> (Int, Float, Float, Float) -> String
    showOneTuple accum (period, pPaid, iPaid, balance) =
      printf "%7d%22.2f%22.2f%22.2f\n%s" period pPaid iPaid balance accum

showUsage progName =
  printf "Usage: %s <loan> <annualInterestRate> <years>\n\
         \For example, %s 400000 4.5 30" progName progName
