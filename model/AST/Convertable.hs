{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}
module Model.AST.Convertable where
import Model.Error



-- Defines the Convertable class, that makes it possible to switch
-- from the Language.Haskell.Exts AST to my custom AST
class Convertable a b where
    convert :: a -> Either Error b
    revert  :: b -> Either Error a
    {-# MINIMAL convert, revert #-}



