module Interp.Impl where

import Control.Monad()

import Ast

replace :: String -> Template -> String
replace _ [] = ""
replace s (x:xs) = case x of
                    St -> s ++ replace s xs
                    Lt l -> l ++ replace s xs
                   
                    
match :: String -> Template -> Maybe (Maybe String)
match "" [] = Just Nothing
match _ [] = Nothing
match s (t:ts) = case t of 
                St -> trymatch s (t:ts)
                Lt l -> case prefix s l of
                         (True, ret) -> match ret ts
                         (False, _) -> Nothing
                
prefix :: String -> String -> (Bool, String)
prefix "" "" = (True, "")
prefix s "" = (True, s)
prefix "" _ = (False, "")
prefix (x:xs) (y:ys) = if x == y
                            then prefix xs ys
                            else (False, "")
                
 
trymatch :: String -> Template -> Maybe (Maybe String)
trymatch s t = trymatch' s s [] t


 
trymatch' :: String -> String -> String -> Template -> Maybe (Maybe String)
trymatch' s _ _ [St] = Just (Just s)
trymatch' _ [] _ _ = Nothing
trymatch' s (x:xs) ys t = if s == replace ys t 
                         then Just (Just ys)
                         else trymatch' s xs (ys ++ [x]) t

check :: Makefile -> Bool
check ms = all (\x -> checkrule x) ms

checkrule :: Rule -> Bool
checkrule (Rule targets prereqs commands) = 
    if (any (\x -> checktemplate x) prereqs) || any (\x -> checktemplate x) commands
    then 
        all (\x -> checktemplate x) targets
    else 
      True

checktemplate :: Template -> Bool
checktemplate t = any (\x -> St == x) t

type Context = (Makefile, [Command], [(String, [Rule])], (File -> Bool), Int)


newtype MF a = MF {
  runMF :: Context -> Maybe (a, Context)
}

instance Functor MF where
  fmap f m = m >>= \a -> return (f a)
  
instance Applicative MF where
  pure = return
  df <*> dx = df >>= \f -> dx >>= return . f
  
instance Monad MF where
  return a = MF $ \ s -> Just (a, s)
  
  m >>= f = MF $ \ s -> do
    case runMF m s of
        Nothing -> Nothing
        Just (a, s') -> runMF (f a) s'
    
  fail _ = MF $ \ _ -> Nothing
  

  
reject :: MF a
reject = MF $ \ _ -> Nothing
  
checkmakefile :: MF ()
checkmakefile = do
    (mf, _, _, _, _) <- get
    if check mf then return ()
                else reject
  
get :: MF Context
get = MF $ \ r -> Just (r, r)
  
set :: Makefile -> [Command] -> [(String, [Rule])] -> (File -> Bool) -> Int -> MF ()
set makefile commands targets function int = 
    MF $ \ _ -> Just ((), (makefile, commands, targets, function, int)) 

getCommands :: MF [Command]
getCommands = do
    (_, c, _, _, _) <- get
    return c
    
getFunction :: MF (File -> Bool)
getFunction = do
    (_, _, _, func, _) <- get
    return func
 
getCounter :: MF Int 
getCounter = do
    (_, _, _, _, i) <- get
    return i
 
decreaseCounter :: MF ()
decreaseCounter = do
    (m, c, s, f, i) <- get
    set m c s f $ i-1

getTargets :: MF [(String, [Rule])]
getTargets = do
    (_, _, targets, _, _) <- get
    return targets
    
addTarget :: String -> Rule -> MF ()
addTarget s r = do
    (m, c, targets, f, i) <- get
    newtargets <- addTarget' s r [] targets
    set m c newtargets f i

addTarget' :: String -> Rule -> [(String, [Rule])] -> [(String, [Rule])] -> MF [(String, [Rule])]
addTarget' s r targets [] = return $ (s, [r]) : targets
addTarget' s r targets ((target, rulelist):ts) =
    if s == target 
    then return $ targets ++ [(target, r:rulelist)] ++ ts
    else 
        addTarget' s r ((target, rulelist):targets) ts

-- Returns False if rule not used for particular target
-- Return       
ruleUsed :: String -> Rule -> MF Bool
ruleUsed s r = do
    targets <- getTargets
    ruleUsed' s r targets
    
ruleUsed' :: String -> Rule -> [(String, [Rule])] -> MF Bool
ruleUsed' _ _ [] = return False
ruleUsed' s r ((target, rules):targets) =
    if s == target
    then return $ r `elem` rules 
    else ruleUsed' s r targets    

    
addCommand :: Command -> MF ()
addCommand command = do
    (mf, commands, s, f, i) <- get
    set mf (command : commands) s f i
    
addCommands :: [Command] -> MF ()
addCommands commands1 = do
    (mf, commands2, s, f, i) <- get
    set mf (commands2 ++ commands1) s f i

build :: Makefile -> File -> (File -> Bool) -> Int -> Maybe [Command]
build mf s f i = case runMF (runBuild s) (mf, [], [], f, i) of
    Just (commands, _) -> Just commands
    Nothing -> Nothing
    

runBuild :: String -> MF [Command]
runBuild s = do
    checkmakefile
    (prereqs, commands) <- getRule s
    newprereqs <- trimPrereqs prereqs
    decreaseCounter
    runBuildRecursion newprereqs
    addCommands commands
    getCommands
    
trimPrereqs :: [String] -> MF [String]
trimPrereqs prereqs = do
    func <- getFunction
    return $ filter (\x -> not $ func x) prereqs
    

    
runBuildRecursion :: [String] -> MF ()
runBuildRecursion [] = return ()
runBuildRecursion prereqs = do
    i <- getCounter
    if i < 1 
    then reject
    else do
       (newprereqs, commands) <- buildTargets prereqs
       decreaseCounter
       runBuildRecursion newprereqs
       addCommands commands

   

buildTargets :: [String] -> MF ([String], [String])
buildTargets [] = return ([], [])
buildTargets (t:ts) = do
     (prereqs1, commands1) <- buildTargets ts
     (prereqs2, commands2) <- getRule t
     return $ (prereqs1 ++ prereqs2, commands1 ++ commands2)
     
     

getRule :: String -> MF ([String], [String])
getRule s = do
    (mf, _, _, _, _) <- get
    getRule' s mf
        where getRule' _ [] = reject
              getRule' s1 (rule:rs) = do
              used <- ruleUsed s1 rule
              if used 
              then 
                getRule' s1 rs
              else
                getRule'' s1 rule rule
                where getRule'' _ (Rule [] _ _) _ = getRule' s rs
                      getRule'' s2 (Rule (t:ts) prereqs commands) r =   
                        case match s2 t of
                        Nothing -> getRule'' s2 (Rule ts prereqs commands) r
                        Just Nothing -> let
                                        tempPrereqs = map (\x -> replace "" x) prereqs
                                        newcommands = map (\x -> replace "" x) commands
                                        in do
                                        addTarget s2 r
                                        newprereqs <- trimPrereqs tempPrereqs
                                        return (newprereqs, newcommands)
                        Just (Just s3) -> let
                                        tempPrereqs = map (\x -> replace s3 x) prereqs
                                        newcommands = map (\x -> replace s3 x) commands
                                        in do
                                        addTarget s2 r
                                        newprereqs <- trimPrereqs tempPrereqs
                                        return (newprereqs, newcommands)
                                       

                                       


