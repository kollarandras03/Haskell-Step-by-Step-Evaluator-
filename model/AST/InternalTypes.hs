{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Model.AST.InternalTypes(State(..), ChangeType(..)) where

-- A state of an expression
{-
UnSafe: let (x:xs) = []
    -> x, and xs has an UnSafe state if used, and Error if evaluated
-}
data State = Change ChangeType  
           | NoChange 
           | Done 
           | UnSafe 
           | Error String deriving Eq

data ChangeType = ExpError String 
                | ExpDone
                | Evaluating
                | PatternMatch
                | NeedsWHNF
                | NewRHS
                | RemovedParen 
                | Substitute
                | ByDef deriving Eq
