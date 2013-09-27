module VData where
 
data VVal = VInt_ Int
          | VStr_ String
          | VSym_ String
          | VBool_ Bool
          | VQuote_ [VVal]
          deriving Eq
 
type VEnv = (String, VFunc)
 
data VContext = VContext_ [VVal] [VEnv]
 
data VFunc = VFunc_ [VVal] [VEnv]
           | VPFunc_ (VContext -> VContext)
           | VIOFunc_ (VContext -> IO VContext)
 
instance Show VFunc where
        show (VFunc_ v e) = "<defined>"
        show (VPFunc_ f) = "<primitive>"

instance Show VVal where
        show (VStr_ s) = "{" ++ (show s) ++ "}"
        show (VInt_ s) = show s
        show (VBool_ True) = "#true"
        show (VBool_ False) = "#false"
        show (VSym_ v) = "$" ++ v
        show (VQuote_ v) = "[" ++ (show v) ++ "]"

instance Show VContext where
        show (VContext_ [] env) = ""
        show (VContext_ y@(x : xs) env) = (show xs) ++ " " ++ (show x)


