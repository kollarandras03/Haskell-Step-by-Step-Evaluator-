{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Model.AST.Pat where
import qualified Language.Haskell.Exts as E
import Model.AST.Convertable
import Model.AST.Names
import Model.AST.Literal
import Model.AST.InternalTypes
import Model.AST.ASTInfo
import Model.Error

--------------------------------------------

data Sign = Pos | Neg deriving Eq

instance Convertable (E.Sign ()) Sign where
    convert (E.Signless _) = Right Pos
    convert (E.Negative _) = Right Neg

    revert Pos = Right $ E.Signless ()
    revert Neg = Right $ E.Negative ()

--------------------------------------------


data Pat = PVar PatInfo Name
         | PApp PatInfo QName [Pat]
         | PInfix PatInfo Pat QName Pat
         | PTuple PatInfo [Pat]
         | PList PatInfo [Pat]
         | PParen Pat
         | PWildCard PatInfo
         | PAsPat PatInfo Name Pat
         | PIrrPat Pat
         | PLit PatInfo Sign Literal deriving (Eq)

defaultPatInfo :: PatInfo
defaultPatInfo = PatInfo NoChange Nothing

instance Convertable (E.Pat ()) Pat where
    convert (E.PIrrPat _ p) = PIrrPat <$> convert p
    convert (E.PVar _ n) = PVar defaultPatInfo <$> convert n
    convert (E.PApp _ q ps) = PApp defaultPatInfo <$> (convert q) <*> traverse convert ps
    convert (E.PInfixApp _ p1 q p2) = PInfix defaultPatInfo <$> (convert p1) <*> (convert q) <*> (convert p2)
    convert (E.PTuple _ _ ps) = PTuple defaultPatInfo <$> traverse convert ps
    convert (E.PList _ ps) = PList defaultPatInfo <$> traverse convert ps
    convert (E.PParen _ p) = PParen <$> (convert p)
    convert (E.PWildCard _) = Right $ PWildCard defaultPatInfo
    convert (E.PAsPat _ n p) = PAsPat defaultPatInfo <$> (convert n) <*> (convert p)
    convert (E.PLit _ s l) = PLit defaultPatInfo <$> (convert s) <*> convert l
    
    convert (E.PNPlusK _ _ _) = Left $ NotSupported
    convert (E.PRec _ _ _) = Left $ NotSupported
    convert (E.PBangPat _ _) = Left $ NotSupported
    convert (E.PQuasiQuote _ _ _) = Left $ NotSupported
    convert _ = Left NotSupported

    revert (PIrrPat p) = E.PIrrPat () <$> revert p
    revert (PVar _ n) = E.PVar () <$> revert n
    revert (PApp _ q ps) = E.PApp () <$> (revert q) <*> traverse revert ps
    revert (PInfix _ p1 q p2) = E.PInfixApp () <$> (revert p1) <*> (revert q) <*> (revert p2)
    revert (PTuple _ ps) = E.PTuple () E.Boxed <$> traverse revert ps
    revert (PList _ ps) = E.PList () <$> traverse revert ps
    revert (PParen p) = E.PParen () <$> revert p
    revert (PWildCard _) = Right $ E.PWildCard ()
    revert (PAsPat _ n p) = E.PAsPat () <$> (revert n) <*> (revert p)
    revert (PLit _ s l) = E.PLit () <$> (revert s) <*> (revert l)
