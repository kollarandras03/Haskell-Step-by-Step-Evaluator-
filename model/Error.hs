module Model.Error where

data Error 
    = NotImplemented
    | NotSupported 
    | InvalidState deriving (Eq, Show, Ord)
