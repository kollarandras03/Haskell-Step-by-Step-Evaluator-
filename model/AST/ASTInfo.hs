module Model.AST.ASTInfo(PatInfo(..), DeclInfo(..), ExpInfo(..),getS, setS) where
import Model.AST.Type
import Model.AST.InternalTypes

data PatInfo = PatInfo State (Maybe EType) deriving Eq

data DeclInfo = DeclInfo State (Maybe EType) deriving Eq

data ExpInfo a
    = State State 
    | ExpInfo State (Maybe Type) 
    | ExpInfoS State (Maybe Type) a deriving Eq

getS :: ExpInfo a -> State
getS (State s) = s
getS (ExpInfo s _) = s
getS (ExpInfoS s _ _) = s

setS :: ExpInfo a -> State -> ExpInfo a
setS (State _) s = State s
setS (ExpInfo _ t) s = ExpInfo s t
setS (ExpInfoS _ t a) s = ExpInfoS s t a