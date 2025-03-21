module Model.AST.Core where
import qualified Language.Haskell.Exts as E
import Model.AST.Convertable
import Model.AST.Type
import Model.AST.Names
import Model.AST.Literal
import Model.AST.Pat
import Model.Error
import Model.AST.InternalTypes
import qualified Model.AST.ASTInfo as I
import Data.Either
import Data.Functor(void)

fromR (Right x) = x


unSafeParseExp :: String -> Exp
unSafeParseExp s = case E.parseExp s of
    E.ParseOk e -> fromR $ convert (void e)
    E.ParseFailed _ m -> error m

unSafeParsePat :: String -> Pat
unSafeParsePat s = case E.parsePat s of
    E.ParseOk p -> fromR $ convert (void p)
    E.ParseFailed _ m -> error m

unSafeParseType :: String -> Type
unSafeParseType s = case E.parseType s of
    E.ParseOk t -> fromR $ convert (void t)
    E.ParseFailed _ m -> error m

unSafeParseDecl :: String -> Decl
unSafeParseDecl s = case E.parseDecl s of
    E.ParseOk d -> fromR (convert (void d))
    E.ParseFailed _ m -> error m



parseExp :: String -> ErrorOr Exp
parseExp s = case E.parseExp s of
    E.ParseOk e -> convert $ void e
    E.ParseFailed _ m -> error m

parsePat :: String -> ErrorOr Pat
parsePat s = case E.parsePat s of
    E.ParseOk p -> convert $ void p
    E.ParseFailed _ m -> error m

parseType :: String -> ErrorOr Type
parseType s = case E.parseType s of
    E.ParseOk t -> convert $ void t
    E.ParseFailed _ m -> error m

parseDecl :: String -> ErrorOr Decl
parseDecl s = case E.parseDecl s of
    E.ParseOk d -> convert $ void d
    E.ParseFailed _ m -> error m


data MatchState = MatchNew | MatchOk | MatchFail | MatchUnk deriving Eq
data RhsState   = RhsOk  | RhsFail  | RhsUnk deriving Eq
data GrhsState  = GrhsOk | GrhsFail | GrhsUnk deriving Eq
data StmtState  = StmtOk | StmtFail | StmtUnk deriving Eq

--------------------------------------------

data Assoc = AssocNone | AssocLeft | AssocRight deriving (Eq)
instance Convertable (E.Assoc ()) Assoc where
    convert :: E.Assoc () -> ErrorOr Assoc
    convert (E.AssocNone _) = Right $ AssocNone
    convert (E.AssocLeft _) = Right $  AssocLeft
    convert (E.AssocRight _) = Right $ AssocRight

    revert :: Assoc -> ErrorOr (E.Assoc ())
    revert AssocNone = Right $ E.AssocNone ()
    revert AssocLeft = Right $ E.AssocLeft ()
    revert AssocRight = Right $ E.AssocRight ()

--------------------------------------------

data Fixity = Fixity Assoc Int QName deriving (Eq)
instance Convertable E.Fixity Fixity where
    convert :: E.Fixity -> ErrorOr Fixity
    convert (E.Fixity a i qn) = Fixity <$> (convert a) <*> (Right i) <*> (convert qn)

    revert :: Fixity -> ErrorOr (E.Fixity)
    revert (Fixity a i qn) = E.Fixity <$> (revert a) <*> (Right i) <*> (revert qn)

--------------------------------------------

type Scope = [Decl]
type ExpInfoSc = I.ExpInfo (Maybe Scope)

data ID = ID Int deriving Eq

data Exp 
    = Var ExpInfoSc QName
    | Con ExpInfoSc QName
    | Lit ExpInfoSc Literal
    | InfixApp ExpInfoSc Exp QOp Exp
    | App ExpInfoSc Exp Exp
    | NegApp ExpInfoSc Exp
    | Lambda ExpInfoSc [Pat] Exp
    | Let ExpInfoSc Binds Exp
    | Tuple ExpInfoSc [Exp]
    | List ExpInfoSc [Exp]
    | Paren ExpInfoSc Exp
    | ExpTypeSig Exp EType
    | VarP ExpInfoSc ID QName deriving Eq
        -- | If State Exp Exp Exp
        -- | MultiIf State [GuardedRhs]
        -- | Case State Exp [Alt]
        -- | LeftSection State Exp QOp
        -- | RightSection State QOp Exp
        -- | EnumFrom ExpInfo Exp
        -- | EnumFromTo ExpInfo Exp Exp
        -- | EnumFromThen ExpInfo Exp Exp
        -- | EnumFromThenTo ExpInfo Exp Exp Exp
        -- | ListComp ExpInfo Exp [QualStmt]


basicExpInfoS :: ExpInfoSc
basicExpInfoS = I.ExpInfoS NoChange Nothing Nothing

basicExpInfo :: ExpInfoSc
basicExpInfo = I.ExpInfo NoChange Nothing

basicState :: ExpInfoSc
basicState = I.State NoChange

instance Convertable (E.Exp ()) Exp where
    convert :: E.Exp () -> ErrorOr Exp
    convert (E.Var _ q) = Var basicExpInfoS <$> convert q
    convert (E.Con _ q) = Con basicExpInfo <$> convert q
    convert (E.Lit _ l) = Lit basicExpInfo <$> convert l
    convert (E.InfixApp _ e1 q e2) = InfixApp basicState <$> (convert e1) <*> (convert q) <*> (convert e2)
    convert (E.App _ e1 e2) = App basicState <$> (convert e1) <*> (convert e2)
    convert (E.NegApp _ e) = NegApp basicState <$> convert e
    convert (E.Lambda _ ps e) = Lambda basicExpInfo <$> (traverse convert ps) <*> convert e
    convert (E.Let _ b e) = Let basicState <$> (convert b) <*> convert e
    convert (E.Tuple _ _ es) = Tuple basicExpInfo <$> traverse convert es
    convert (E.List _ es) = List basicExpInfo <$> traverse convert es
    convert (E.Paren _ e) = Paren basicState <$> convert e
    convert (E.ExpTypeSig _ e t) = ExpTypeSig <$> (convert e) <*> (Right t)
    convert _ = Left NotSupported

    revert :: Exp -> ErrorOr (E.Exp ())
    revert (Var _ q) = E.Var () <$> revert q
    revert (Con _ q) = E.Con () <$> revert q
    revert (Lit _ l) = E.Lit () <$> revert l
    revert (InfixApp _ e1 q e2) = E.InfixApp () <$> (revert e1) <*> (revert q) <*> (revert e2)
    revert (App _ e1 e2) = E.App () <$> (revert e1) <*> (revert e2)
    revert (NegApp _ e) = E.NegApp () <$> revert e
    revert (Lambda _ ps e) = E.Lambda () <$> (traverse revert ps) <*> revert e
    revert (Let _ b e) = E.Let () <$> (revert b) <*> revert e
    revert (Tuple _ es) = E.Tuple () E.Boxed <$> traverse revert es
    revert (List _ es) = E.List () <$> traverse revert es
    revert (Paren _ e) = E.Paren () <$> revert e
    revert (ExpTypeSig e t) = E.ExpTypeSig () <$> (revert e) <*> (Right t)





--------------------------------------------

data Alt = Alt Pat Rhs (Maybe Binds) deriving (Eq)
instance Convertable (E.Alt ()) Alt where
    convert :: E.Alt () -> ErrorOr Alt
    convert (E.Alt _ p r b) = Alt <$> (convert p) <*> (convert r) <*> traverse convert b

    revert :: Alt -> ErrorOr (E.Alt ())
    revert (Alt p r b) = E.Alt () <$> (revert p) <*> (revert r) <*> traverse revert b

--------------------------------------------

data Binds = BDecls [Decl] deriving Eq
instance Convertable (E.Binds ()) Binds where
    convert :: E.Binds () -> ErrorOr Binds
    convert (E.BDecls _ ds) = BDecls <$> traverse convert ds

    revert :: Binds -> ErrorOr (E.Binds ())
    revert (BDecls ds) = E.BDecls () <$> traverse revert ds

--------------------------------------------

basicDeclInfo :: I.DeclInfo
basicDeclInfo = I.DeclInfo NoChange Nothing

data Decl 
    = TypeSig [Name] (E.Type ())
    | FunBind I.DeclInfo [Match]
    | PatBind I.DeclInfo Pat Rhs (Maybe Binds) deriving (Eq)

instance Convertable (E.Decl ()) Decl where
    convert :: E.Decl () -> ErrorOr Decl
    convert (E.TypeSig _ ns t) = TypeSig <$> (traverse convert ns) <*> (Right t)
    convert (E.FunBind _ ms) = FunBind basicDeclInfo <$> traverse convert ms
    convert (E.PatBind _ p r b) = PatBind basicDeclInfo <$> (convert p) <*> (convert r) <*> traverse convert b
    convert (E.DataDecl _ _ _ _ _ _) = Left NotImplemented
    convert (E.InstDecl _ _ _ _) = Left NotImplemented
    convert _ = Left NotSupported

    revert :: Decl -> ErrorOr (E.Decl ())
    revert (TypeSig ns t) = E.TypeSig () <$> (traverse revert ns) <*> (Right t)
    revert (FunBind _ ms) = E.FunBind () <$> traverse revert ms
    revert (PatBind _ p r b) = E.PatBind () <$> (revert p) <*> (revert r) <*> traverse revert b

--------------------------------------------

data Match = Match MatchState Name [Pat] Rhs (Maybe Binds) deriving Eq
instance Convertable (E.Match ()) Match where
    convert :: E.Match () -> ErrorOr Match
    convert (E.Match _ n ps r b) = Match MatchNew <$> (convert n) <*> (traverse convert ps) <*> (convert r) <*> traverse convert b
    convert (E.InfixMatch _ p n ps r b) = Left NotSupported
    
    revert :: Match -> ErrorOr (E.Match ())
    revert (Match _ n ps r b) = E.Match () <$> (revert n) <*> (traverse revert ps) <*> (revert r) <*> traverse revert b

--------------------------------------------

data Rhs = UnGuardedRhs RhsState Exp | GuardedRhss RhsState [GuardedRhs] deriving (Eq)
instance Convertable (E.Rhs ()) Rhs where
    convert :: E.Rhs () -> ErrorOr Rhs
    convert (E.UnGuardedRhs _ e) = UnGuardedRhs RhsUnk <$> convert e
    convert (E.GuardedRhss _ gs) = GuardedRhss RhsUnk <$> traverse convert gs

    revert :: Rhs -> ErrorOr (E.Rhs ())
    revert (UnGuardedRhs _ e) = E.UnGuardedRhs () <$> revert e
    revert (GuardedRhss _ gs) = E.GuardedRhss () <$> traverse revert gs

--------------------------------------------

data Stmt = Qualifier StmtState Exp deriving Eq

instance Convertable (E.Stmt ()) Stmt where
    convert :: E.Stmt () -> ErrorOr Stmt
    convert (E.Generator _ p e) = Left NotSupported
    convert (E.Qualifier _ e) = Qualifier StmtUnk <$> convert e
    convert (E.LetStmt _ b) = Left NotSupported
    convert _ = Left NotSupported

    revert :: Stmt -> ErrorOr (E.Stmt ())
    revert (Qualifier _ e) = E.Qualifier () <$> revert e

--------------------------------------------

data GuardedRhs = GuardedRhs GrhsState [Stmt] Exp deriving (Eq)
instance Convertable (E.GuardedRhs ()) GuardedRhs where
    convert :: E.GuardedRhs () -> ErrorOr GuardedRhs
    convert (E.GuardedRhs _ ss e) = GuardedRhs GrhsUnk <$> (traverse convert ss) <*> (convert e)

    revert :: GuardedRhs -> ErrorOr (E.GuardedRhs ())
    revert (GuardedRhs _ ss e) = E.GuardedRhs () <$> (traverse revert ss) <*> revert e

--------------------------------------------

data QualStmt = QualStmt Stmt deriving (Eq)
instance Convertable (E.QualStmt ()) QualStmt where
    convert :: E.QualStmt () -> ErrorOr QualStmt
    convert (E.QualStmt _ s) = QualStmt <$> convert s
    convert _ = Left NotSupported

    revert :: QualStmt -> ErrorOr (E.QualStmt ())
    revert (QualStmt s) = E.QualStmt () <$> revert s

--------------------------------------------

data QOp 
    = QVarOp ExpInfoSc QName 
    | QConOp ExpInfoSc QName 
    | QVArOPP ExpInfoSc ID QName deriving Eq

instance Convertable (E.QOp ()) QOp where
    convert :: E.QOp () -> ErrorOr QOp
    convert (E.QVarOp _ q) = QVarOp basicExpInfoS <$> convert q
    convert (E.QConOp _ q) = QConOp basicExpInfo <$> convert q

    revert :: QOp -> ErrorOr (E.QOp ())
    revert (QVarOp _ q) = E.QVarOp () <$> revert q
    revert (QConOp _ q) = E.QConOp () <$> revert q

--------------------------------------------