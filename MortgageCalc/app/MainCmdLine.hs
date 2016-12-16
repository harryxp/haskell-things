import Flow ((|>))
import System.Environment (getArgs, getProgName)
import Text.Printf

import MortgageCalc (calcMonthlyPayment, calcAmortization)

outputStr::Float -> Int -> [(Int, Float, Float, Float)] -> String
outputStr p n a = concat
  [ "Detailed amortization:\n"
  , printf "%7s%22s%22s%22s\n" "Month #" "Principal" "Interest" "Balance"
  , showAmortization a
  , "\n"
  , "Your monthly payment is $"
  , show p
  , ", and the total payment is $"
  , show (p * 12 * fromIntegral n)
  , ".\n" ]
  where
    showAmortization::[(Int, Float, Float, Float)] -> String
    showAmortization = foldl showOneTuple ""
    showOneTuple::String -> (Int, Float, Float, Float) -> String
    showOneTuple accum (period, pPaid, iPaid, balance) =
      printf "%7d%22.2f%22.2f%22.2f\n%s" period pPaid iPaid balance accum

usage progName =
  concat ["Usage ./", progName, " <loan> <annualInterestRate> <years>"]

main =
  getArgs                  >>=
  \args     -> getProgName >>=
  \progName -> case args of
    [loan, annualInterestRate, years] ->
      let l = read loan
          i = read annualInterestRate
          n = read years
          p = (calcMonthlyPayment l i n)
          a = (calcAmortization p l i n)
      in
        outputStr p n a |> putStrLn
    otherwise                         -> usage progName |> putStrLn

