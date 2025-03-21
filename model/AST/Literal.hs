{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Model.AST.Literal(Literal(..)) where
import qualified Language.Haskell.Exts as E
import Model.AST.Convertable
import Model.Error

data Literal = Char     Char     
             | String   String   
             | Int      Int  
             | Double   Double 
             | Integer  Integer
             | Float    Float
             | Whole    Integer
             | Frac     Rational deriving (Eq)

instance Convertable (E.Literal ()) Literal where
  convert (E.Char _ c _) = Right $ Char c
  convert (E.String _ s _) = Right $ String s
  convert (E.Int _ i _) = Right $ Whole i
  convert (E.Frac _ r _) = Right $ Frac r
  convert _ = Left NotSupported
  
  revert (Char c) = Right $ E.Char () c (show c)
  revert (String s) = Right $ E.String () s (show s)
  revert (Whole i) = Right $ E.Int () i (show i)
  revert (Frac r) = Right $ E.Frac () r (show r)
  revert (Int i) = Right $ E.Int () (fromIntegral i) (show i)
  revert (Double d) = Right $ E.Frac () (toRational d) (show d)
  revert (Integer i) = Right $ E.Int () (fromIntegral i) (show i)
  revert (Float f) = Right $ E.Frac () (toRational f) (show f)