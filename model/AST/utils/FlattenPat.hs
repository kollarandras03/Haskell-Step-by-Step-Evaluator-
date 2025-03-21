module Model.AST.Utils.FlattenPat(flatten,flattenN,flattenS) where
import Model.AST.AST

flatten :: Pat -> [Pat]
flatten p@(PVar {}) = [p]
flatten p@(PLit {}) = []
flatten   (PWildCard _) = []
flatten p@(PInfix _ p1 _ p2) = flatten p1 <> flatten p2
flatten   (PParen p) = flatten p
flatten   (PApp _ _ pats) = concatMap flatten pats
flatten   (PList _ pats) = concatMap flatten pats
flatten   (PTuple _ pats) = concatMap flatten pats
flatten   (PAsPat l name pat) = PVar l name : flatten pat
flatten   (PIrrPat pat) = flatten pat

flattenN :: Pat -> [Name]
flattenN = map (\(PVar _ n) -> n) . flatten

flattenS :: Pat -> [String]
flattenS p = map nameToStr (flattenN p) where
    nameToStr (Ident x) = x
    nameToStr (Symbol x) = x