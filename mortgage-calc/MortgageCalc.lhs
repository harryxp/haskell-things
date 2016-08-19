Consider a general situation of mortgage: you borrow L dollars from a bank to
buy a house, the annual interest rate is I, and it's an N-year fixed-rate loan,
how much do you pay each month?

Let's represent monthly payment with P and consider a special case first.  If
we borrow the money for only one month, we have

  L * (1 + I/12) - P = 0

That is,

  L = P / (1 * I/12)

which makes sense - the payment divided by (1 + monthly interest) is equal to
the amount of money borrowed.

If we borrow L for 2 months -

  (L * (1 + I/12) - P) * (1 + I/12) - P = 0

Note that (L * (1 + I/12) - P) is what's left after the first month's payment.
In the end it's always going to be equal to 0 because you pay off the loan plus
any interest.  This is equivalent to

  L = P / (1 + I/12) + P / (1 + I/12) ^ 2

This goes on and on.  So for an N-year loan, you have 12N periods, that is

  L = P / (1 + I/12) + ... + P / (1 + I/12) ^ (12N - 1) + P / (1 + I/12) ^ 12N

Note again, that L is equal to the sum of the present value of all your monthly
payment.  Pretty amazing.

Essentially we want P represented by L, I and N.  It's a geometric series.

Let r = 1 / (1 + I/12),

  L = r * P + ... + r ^ (12N - 1) * P + r ^ 12N * P

and multiply r on both sides
  r * L = r ^ 2 * P + ... + r ^ 12N * P + r ^ (12N + 1) * P

This subtracts the original equation -

(r - 1) * L = r ^ (12N + 1) * P - r * P

So finally, monthly payment P is

  P = (r - 1) / (r ^ (12N + 1) - r) * L

> import System.Environment (getArgs, getProgName)
> import Text.Printf

> calcMonthlyPayment::Float -> Float -> Int -> Float
> calcMonthlyPayment l i n = (r - 1) / (r ^ (12 * n + 1) - r) * l
>   where
>     r = 1 / (1 + i / 12)

Now that L, I, N and P are all known, let's look at amortization.  That is, for
each month, how much principal is paid off and how much interest is paid.

At month 0, L is the balance.
At month 1, L * (1 + I/12) is the balance, before payment.
At month 2, (L * (1 + I/12) - P) * (1 + I/12) is the balance before payment.
This amount minus the balance from last month, is how much is paid off in
principal.
At month 3, ((L * (1 + I/12) - P) * (1 + I/12) - P) * (1 + I/12) is the balance
before payment.  This amount minus the balance from last month, is how much is
paid off in principal.
...

> calcAmortization::Float -> Float -> Float -> Int -> [(Int, Float, Float, Float)]
> calcAmortization p l i n = _calcAmortization 0 l [(0, 0, 0, l)]
>   where
>     _calcAmortization period balance tuples =
>       if period >= n * 12
>       then tuples
>       else
>         let interestPaidOfMonth = balance * (i / 12)
>             principalPaidOfMonth = p - interestPaidOfMonth
>             newBalance = balance * (1 + i / 12) - p
>         in
>             _calcAmortization
>               (period + 1)
>               newBalance
>               ((period + 1, principalPaidOfMonth, interestPaidOfMonth, newBalance) : tuples)

Now we just need to print out things nicely...

> outputStr::Float -> Int -> [(Int, Float, Float, Float)] -> String
> outputStr p n a = concat
>   [ "Your monthly payment is $"
>   , show p
>   , ", and the total payment is $"
>   , show (p * 12 * fromIntegral n)
>   , ".\n"
>   , "Detailed amortization:\n"
>   , printf "%7s%22s%22s%22s\n" "Month #" "Principal" "Interest" "Balance"
>   , showAmortization a ]
>   where
>     showAmortization::[(Int, Float, Float, Float)] -> String
>     showAmortization = foldl showOneTuple ""
>     showOneTuple::String -> (Int, Float, Float, Float) -> String
>     showOneTuple accum (period, pPaid, iPaid, balance) =
>       printf "%7d%22.2f%22.2f%22.2f\n%s" period pPaid iPaid balance accum

> usage progName =
>   concat ["Usage ./", progName, " <loan> <annualInterestRate> <years>"]

> main =
>   getArgs                  >>=
>   \args     -> getProgName >>=
>   \progName -> case args of
>     [loan, annualInterestRate, years] ->
>       let l = read loan
>           i = read annualInterestRate
>           n = read years
>           p = (calcMonthlyPayment l i n)
>           a = (calcAmortization p l i n)
>       in
>         putStrLn $ outputStr p n a
>     otherwise                         -> putStrLn $ usage progName
