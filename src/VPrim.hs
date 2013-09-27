module VPrim where
import VData
import Debug.Trace

-- | v "[a 1] . [b a] . [a 100] . b b + "
apply :: VFunc -> VContext -> IO VContext
apply fn c@(VContext_ stack env)
  = do r <-  case fn of
                  VIOFunc_ f -> f c
                  VPFunc_ f -> return (f c)
                  VFunc_ f e -> do w <- walkerV f (VContext_ stack e)
                                   w1 <- case w of
                                              VContext_ nstack nenv -> return (VContext_ nstack env)
                                   return w1
       return r

walkerV :: [VVal] -> VContext -> IO VContext
walkerV [] s = return s
walkerV q@(v : vs) s = do y <- (walk v s)
                          walkerV vs y

walk :: VVal -> VContext -> IO VContext
walk v@(VSym_ i) c@(VContext_ s e)
  = case (lookup i e) of
        Just fn -> apply fn c
        Nothing -> return $ error $ "Error lookup<" ++ (show i) ++ ">"
walk q (VContext_ s env) = return (VContext_ (q : s) env)



-- Debug functions
vDebug vc@(VContext_ s e)
  = trace ("stack: " ++ (show s) ++ "\nenv: " ++ (show e)) vc

-- Arithmetic
vAdd (VContext_ s e)
  = case s of
        (VInt_ x) : (VInt_ y) : z -> VContext_ ((VInt_ (y + x)) : z) e
vSub (VContext_ s e)
  = case s of
        (VInt_ x) : (VInt_ y) : z -> VContext_ ((VInt_ (y - x)) : z) e
vMul (VContext_ s e)
  = case s of
        (VInt_ x) : (VInt_ y) : z -> VContext_ ((VInt_ (y * x)) : z) e


-- Stack manipulations
-- vI is eval i.e 1 2 [3 4] i => 1 2 3 4
-- vI is eval i.e 1 2 [3 4 +] i => 1 2 7
vI full@(VContext_ s e)
  = case s of
         ((VQuote_ v):xs) -> walkerV v (VContext_ xs e)

-- 1 2 dup => 1 2 2
vDup (VContext_ s e)
  = case s of
         (x:xs) -> VContext_ (x:x:xs) e
-- 1 2 swap => 2 1
vSwap (VContext_ s e)
  = case s of
         (x1:x2:xs) -> VContext_ (x2:x1:xs) e


-- TODO: disable redefinitions in the same scope.

-- vdef takes stack of the form [mydefname my def impl] .
-- our vcontext(s) is a list. so we take the head of the list to extract
-- the first element i.e [mydef aa bb cc] . 1 2 3 => 1 2 3
-- then take the tail of the quote, then bind them to the env, and return
-- back with our tail of vcontext(s) and new env.
vDef (VContext_ s e)
  = case (head s) of
        VQuote_ q -> VContext_ (tail s) env
          where quote = tail q
                name = case (head q) of
                        VSym_ s -> s
                env = (name, (VFunc_ quote e)) : e

vPuts full@(VContext_ s e)
  = case s of
         (x:xs) -> do putStrLn (show x)
                      return (VContext_ xs e)


-- TODO: make this into a symbol table with the strings changed to 
-- integer entries for lookups.
-- prim = [(".", VPFunc_ vDef)]
prim  = [("+", VPFunc_ vAdd), ("-", VPFunc_ vSub), ("*", VPFunc_ vMul),
        ("i", VIOFunc_ vI),
        ("puts", VIOFunc_ vPuts),
        ("dup", VPFunc_ vDup),
        ("swap", VPFunc_ vSwap),
     ("?", VPFunc_ vDebug), (".", VPFunc_ vDef) ]

