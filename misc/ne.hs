import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (getOpt, usageInfo, OptDescr(Option), ArgDescr(ReqArg), ArgOrder(RequireOrder))
import System.Eval.Haskell (eval, Import)

main :: IO ()
main  = handleInput >>= evalWithLines >>= handleOutput

handleInput :: IO (String, [String])
handleInput =  getProgName >>= \progName -> getArgs >>= \args ->
  let optDesc = Option ['e'] ["eval"] (ReqArg id "CODE") "Apply code to each line of stdin."
      header = "\nUsage: runghc " ++ progName ++ " [OPTION...]"
  in
    case getOpt RequireOrder [optDesc] args of
      ([o],_,[]) -> getContents >>= \xs -> return (o, (lines xs))
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header [optDesc]))

evalWithLines                :: (String, [String]) -> IO [String]
evalWithLines (codeStr, lns) = eval codeStr [] >>=
  \maybeCode -> case maybeCode of
    Just code -> (return . map code) lns
    Nothing   -> error "Supplied code failed to pass typechecking."

handleOutput        :: [String] -> IO ()
handleOutput output =  mapM putStrLn output >> return ()

-- runghc ne.hs -e 'take 1 :: String -> String' < text

