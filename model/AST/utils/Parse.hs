module Model.AST.Utils.Parse where
import Model.AST.AST

parseExpUnsafe  = (\(Right e) -> e) . parseExp 
parseDeclUnsafe = (\(Right e) -> e) . parseDecl
parsePatUnsafe  = (\(Right e) -> e) . parsePat 