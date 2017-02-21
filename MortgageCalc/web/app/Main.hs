module Main (main) where

import Control.Monad.IO.Class (MonadIO(..))
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (getElementByIdUnsafe)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.HTMLInputElement (getValueUnsafe, setValue)
import GHCJS.DOM.Types (HTMLInputElement(..), unsafeCastTo)
import MortgageCalc (calcMonthlyPayment, calcAmortization)

import qualified GHCJS.DOM.Element as E (click)

main :: IO ()
main = do
  Just doc       <- currentDocument
  -- input
  loanInput      <- (getElementByIdUnsafe doc "loan" >>= unsafeCastTo HTMLInputElement)
  yearInput      <- (getElementByIdUnsafe doc "year" >>= unsafeCastTo HTMLInputElement)
  interestInput  <- (getElementByIdUnsafe doc "interest" >>= unsafeCastTo HTMLInputElement)
  calcButton     <- getElementByIdUnsafe doc "calc"
  -- output
  monthlyPayment <- (getElementByIdUnsafe doc "monthly_payment" >>= unsafeCastTo HTMLInputElement)
  totalPayment   <- (getElementByIdUnsafe doc "total_payment" >>= unsafeCastTo HTMLInputElement)

  on calcButton E.click $ do
    loan     <- getValueUnsafe loanInput
    interest <- getValueUnsafe interestInput
    year     <- getValueUnsafe yearInput
    let monthly = calcMonthlyPayment (read loan) (read interest) (read year) in
      setValue' monthlyPayment ((Just . show) monthly) >>
      setValue' totalPayment ((Just . show) (monthly * 12 * (read year)))

  return ()

setValue' :: (MonadIO m) => HTMLInputElement -> Maybe String -> m ()
setValue' = setValue
