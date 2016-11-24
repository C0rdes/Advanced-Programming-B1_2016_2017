module SubsInterpreter
       ( runProg
       , Error (Error)
       , Value(IntVal, UndefinedVal, TrueVal, FalseVal, StringVal, ArrayVal)
       )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", strictlyEqual)
                       , ("<", lessThan)
                       , ("+", add)
                       , ("*", multiply)
                       , ("-", mySubtract)
                       , ("%", modulo)
                       , ("Array.new", arrayNew)
                       ]
                   
                       
newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}
    
instance Functor SubsM where
  fmap f m = m >>= \a -> return (f a)

instance Applicative SubsM where
  pure = return
  (<*>) = ap

instance Monad SubsM where
  -- return a :: a -> SubsM a
  return x = SubsM $ \s -> Right (x, fst s)
  -- (>>=) m f :: SubsM a -> (a -> SubsM b) -> SubsM b
  f >>= m = SubsM $ \s -> 
    case runSubsM f s of 
      Left e -> Left e
      Right (result, env) -> runSubsM (m result) s
  fail s = SubsM $ \x -> Left (Error s)



arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(take n $ repeat UndefinedVal)
arrayNew _ = fail "Array.new called with wrong number of arguments"

add :: Primitive
add [IntVal v1, IntVal v2] = return $ IntVal $ v1 + v2
add [StringVal s1, StringVal s2] = return $ StringVal $ s1 ++ s2
add [IntVal v1, StringVal s1] = return $ StringVal $ show v1 ++ s1
add [StringVal s1, IntVal v1] = return $ StringVal $ s1 ++ show v1
add [IntVal _, _] = fail "Integers can only be added to strings or integers"
add [StringVal _, _] = fail "String can only be added to strings"
add _ = fail "add called with wrong number of elements" 

lessThan :: Primitive
lessThan [IntVal v1, IntVal v2] = return $ if v1 < v2 then TrueVal else FalseVal
lessThan [StringVal s1, StringVal s2] = return $ if s1 < s2 then TrueVal else FalseVal
lessThan [IntVal _, _] = fail "Integers can only be compared to integers"
lessThan [StringVal _, _] = fail "String can only be compared to strings"
lessThan _ = fail "lessThan called with wrong number of elements"  

multiply :: Primitive
multiply [IntVal v1, IntVal v2] = return $ IntVal $ v1 * v2
multiply _ = fail "multiply called with wrong arguments"
 
strictlyEqual :: Primitive
strictlyEqual [IntVal v1, IntVal v2] = return $ if v1 == v2 then TrueVal else FalseVal
strictlyEqual [StringVal s1, StringVal s2] = return $ if s1 == s2 then TrueVal else FalseVal
strictlyEqual [TrueVal, TrueVal] = return TrueVal
strictlyEqual [FalseVal, FalseVal] = return TrueVal
strictlyEqual [TrueVal, FalseVal] = return FalseVal
strictlyEqual [FalseVal, TrueVal] = return FalseVal
strictlyEqual [UndefinedVal, UndefinedVal] = return FalseVal
strictlyEqual [ArrayVal a1, ArrayVal a2] = return $ if a1 == a2 then TrueVal else FalseVal
strictlyEqual _ = fail "strictlyEqual called with different argument types" 
 
mySubtract :: Primitive
mySubtract [IntVal v1, IntVal v2] = return $ IntVal $ v1 - v2 
mySubtract [_, _] = fail "subtract called with one or more wrong argument types"
mySubtract _ = fail "substract called with wrong number of arguments"
 
modulo :: Primitive
modulo [IntVal v1, IntVal v2]= return $ IntVal $ v1 `mod` v2
modulo [_, _] = fail "modulo called with wrong argument types"
modulo _ = fail "modulo called with wrong number of arguments"
 
modify :: (Env -> Env) -> SubsM ()
modify f = SubsM $ \context -> Right ((), f $ fst context)

updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = SubsM $ \context -> Right((), Map.insert name val $ fst context)

getVar :: Ident -> SubsM Value
getVar name = SubsM $ (\context -> 
                case Map.lookup name $ fst context of
                  Just a -> Right (a, fst context)
                  Nothing -> Left $ Error $ "variable " ++ name ++ " not declared yet")

getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM $ (\context -> 
                     case Map.lookup name $ snd context of
                       Just a -> Right (a, fst context)
                       Nothing -> Left $ Error $ "function " ++ name ++ " not declared")

evalExpr :: Expr -> SubsM Value
evalExpr expr = undefined

stm :: Stm -> SubsM ()
stm s = undefined

program :: Program -> SubsM ()
program (Prog prog) = undefined

runProg :: Program -> Either Error Env
runProg prog = undefined
