import Control.Monad.Except
import Data.Char (chr, ord)
import qualified Data.Map as M
import Text.Read (readMaybe)

type Interpreted = ExceptT String IO [Value]

data Value = Number Double
           | Symbol String
           | Func [Expr]
           | Prim (Maybe Int) ([Value] -> ExceptT String IO [Value])

instance Show Value where
    show (Number n)
        | n == fromIntegral (round n) = show (round n)
        | otherwise                   = show n
    show (Symbol s) = s
    show (Func _)   = "<function>"
    show (Prim _ _) = "<primative>"

data Expr = Literal Value
          | Call

instance Show Expr where
    show (Literal v) = show v
    show Call        = "$"

readFunc :: Int -> [String] -> [String] -> Either String [Expr]
readFunc 0 res xss       = (:) <$> ((Literal . Func) <$> readExprs (reverse $ tail res)) <*> readExprs xss
readFunc _ _ []          = Left "unclosed function"
readFunc d res ("(":xss) = readFunc (d+1) ("(":res) xss
readFunc d res (")":xss) = readFunc (d-1) (")":res) xss
readFunc d res (xs:xss)  = readFunc d (xs:res) xss

readExprs :: [String] -> Either String [Expr]
readExprs []        = Right []
readExprs ("#":xss) = (Call :) <$> readExprs xss
readExprs ("(":xss) = readFunc 1 [] xss
readExprs (")":xss) = Left "unexpected }"
readExprs (xs:xss) = 
    case readMaybe xs :: (Maybe Double) of
      Just n  -> ((Literal $ Number n) :) <$> readExprs xss
      Nothing -> ((Literal $ Symbol xs) :) <$> readExprs xss

parse :: String -> Either String [Expr]
parse = readExprs . words . space
    where space [] = []
          space ('(':xs) = " ( " ++ space xs
          space (')':xs) = " ) " ++ space xs
          space (x:xs)   = x : space xs

interpretExpr :: M.Map String Value -> [Value] -> Expr -> Interpreted
interpretExpr env stk (Literal (Symbol s)) = 
    case M.lookup s env of
      Just v  -> return $ v : stk
      Nothing -> throwError $ "symbol " ++ s ++ " does not exist in this context"
interpretExpr env stk (Literal val) = return $ val : stk
interpretExpr env ((Func exp):stk) Call = interpret env stk exp
interpretExpr env ((Prim n f):stk) Call =
    case n of
      Just n  -> ((++ drop n stk) . reverse) <$> f (reverse $ take n stk)
      Nothing -> f stk
interpretExpr env (x:stk) Call = 
    throwError $ "attempt to call uncallable value: " ++ show x
interpretExpr env [] Call = throwError $ "attempt to call with empty stack"

interpret :: M.Map String Value -> [Value] -> [Expr] -> Interpreted
interpret env stk = foldM (interpretExpr env) stk 

binOp :: (Double -> Double -> Double) -> Value
binOp f = Prim (Just 2) $ \[Number x, Number y] -> return [Number $ f x y]

compOp :: (Double -> Double -> Bool) -> Value
compOp f = Prim (Just 2) $ \[Number x, Number y] -> return [Number $ if f x y then 1 else 0]

defaultEnv = M.fromList $
               [ ("dup", Prim (Just 1) $ \[x] -> return [x, x])
               , ("drop", Prim (Just 1) $ \[_] -> return [])
               , ("swap", Prim (Just 2) $ \[x, y] -> return [y, x])
               , ("over", Prim (Just 2) $ \[x, y] -> return [x, y, x])
               , ("rot", Prim (Just 3) $ \[x, y, z] -> return [y, z, x])
               , (">", compOp (>))
               , ("<", compOp (<))
               , (">=", compOp (>=))
               , ("<=", compOp (<=))
               , ("+", binOp (+))
               , ("-", binOp (-))
               , ("*", binOp (*))
               , ("/", binOp (/))
               , ("^", binOp (**))
               , (".", Prim (Just 2) $ \[Func a, Func b] -> return [Func $ a ++ b])
               , ("if", Prim (Just 3) $ \[Number n, x, y] -> return [if n == 0 then x else y])
               , ("get_chr", Prim (Just 0) $ \[] -> do a <- lift getChar; return [Number $ fromIntegral $ ord a])
               , ("put_chr", Prim (Just 1) $ \[Number n] -> do lift $ putChar (chr $ floor n); return [])
               ]

eval :: String -> IO (Either String [Value])
eval = either (return . Left) (runExceptT . interpret defaultEnv []) . parse

