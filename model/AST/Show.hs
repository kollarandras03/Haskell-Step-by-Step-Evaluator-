module Model.AST.Show where
import Model.AST.Type
import Model.AST.Pat
import Model.AST.Literal
import Model.AST.Core
import Model.AST.Names
import Model.AST.InternalTypes
import qualified Model.AST.ASTInfo as I
import qualified Language.Haskell.Exts(prettyPrint)

instance Show ChangeType where
  show (ExpError m) = "🔴 " <> m
  show ExpDone = "🟢"
  show Evaluating = "🟡"
  show PatternMatch = "🔵"
  show NeedsWHNF = undefined
  show NewRHS = "🔵"

instance Show State where
  show Done = "✅"
  show (Error m) = "❌ " <> m
  show NoChange = ""
  show (Change c) = show c

instance Show ModuleName where
  show (ModuleName n) = "module " <> n

instance Show I.PatInfo where
  show (I.PatInfo s (Just t)) = show s <> " :: " <> show t
  show (I.PatInfo s _) = show s

instance Show I.DeclInfo where
  show (I.DeclInfo s Nothing) = show s
  show (I.DeclInfo s (Just t)) = show s <> " :: " <> (Language.Haskell.Exts.prettyPrint t)

instance (Show a) => Show (I.ExpInfo a) where
  show (I.State s) = show s
  show (I.ExpInfo s _) = show s
  show (I.ExpInfoS s _ sc) = show s <> " " <> show sc

instance Show ID where
  show (ID i) = show i

deriving instance Show Decl 
deriving instance Show Exp
deriving instance Show Pat 
deriving instance Show Type 
deriving instance Show QName
deriving instance Show Name
deriving instance Show SpecialCon
deriving instance Show QOp
deriving instance Show Rhs
deriving instance Show Match
deriving instance Show Binds
deriving instance Show MatchState
deriving instance Show RhsState
deriving instance Show GuardedRhs
deriving instance Show GrhsState
deriving instance Show Stmt
deriving instance Show StmtState
deriving instance Show Sign
deriving instance Show Literal
deriving instance Show Alt
deriving instance Show Fixity
deriving instance Show Assoc 
deriving instance Show QualStmt
deriving instance Show Op  