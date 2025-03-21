module Model.AST.Utils.ManipulateAST where
import Model.AST.AST

qOpToOp :: QOp -> Exp
qOpToOp (QVarOp s n) = Var s n
qOpToOp (QConOp s n) = Con s n

opToQOp (Var s n) = QVarOp s n
opToQOp (Con s n) = QConOp s n
