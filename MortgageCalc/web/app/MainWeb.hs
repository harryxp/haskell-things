module Main (main) where

import MortgageCalc (calcMonthlyPayment, calcAmortization)

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

import GHCJS.DOM (syncPoint, currentDocument)
import GHCJS.DOM.Types
       (HTMLInputElement(..), HTMLParagraphElement(..), HTMLSpanElement(..), JSString, FromJSString, fromJSString, toJSString, unsafeCastTo)
import GHCJS.DOM.Document (getBody, createElementUnsafe, createTextNode)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.HTMLInputElement (getValueAsNumber, getValueUnchecked, setType)
import GHCJS.DOM.EventM (on, mouseClientXY)
import qualified GHCJS.DOM.Document as D (click)
import qualified GHCJS.DOM.Element as E (click)

import qualified Data.Text as T (unpack, Text)

main = do
  Just doc <- currentDocument
  Just body <- getBody doc

  loanInput <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement
  interestInput <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement
  yearInput <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement
  calcBtn <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement
  setType calcBtn (toJSString "button")
  output <- createElementUnsafe doc (Just "input") >>= unsafeCastTo HTMLInputElement

  appendChild body (Just loanInput)
  appendChild body (Just interestInput)
  appendChild body (Just yearInput)
  appendChild body (Just calcBtn)

  on calcBtn E.click $ do
    loan <- getValueAsNumber loanInput
    interest <- getValueAsNumber interestInput
    year <- getValueAsNumber yearInput
    span <- createElementUnsafe doc (Just "span") >>= unsafeCastTo HTMLSpanElement
    output <- createTextNode doc $ show year -- $ calcMonthlyPayment loan interest (round year)
    appendChild span output
    appendChild body (Just span)
    return ()

    {-
    setInnerHTML body (Just "<h1>Kia ora (Hi)</h1>")
    on doc D.click $ do
        (x, y) <- mouseClientXY
        newParagraph <- createElementUnsafe doc (Just "p") >>= unsafeCastTo HTMLParagraphElement
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- createElementUnsafe doc (Just "span") >>= unsafeCastTo HTMLSpanElement
    text <- createTextNode doc "Click here to exit"
    appendChild exit text
    appendChild body (Just exit)
    on exit E.click $ liftIO $ putMVar exitMVar ()

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    setInnerHTML body (Just "<h1>Ka kite ano (See you later)</h1>")
    -}
  return ()
