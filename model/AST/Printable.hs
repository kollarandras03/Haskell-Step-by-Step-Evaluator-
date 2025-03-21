module Model.AST.Printable(Printable(..), printIt,sleep) where
import Model.AST.Core
import Model.AST.InternalTypes
import Model.AST.Literal
import Model.AST.Names
import Model.AST.ASTInfo
import Model.AST.Pat
import Model.AST.Type
import Model.AST.Show
import Model.AST.Module
import Data.List(intercalate)
import qualified Data.Set as Set
import System.Console.ANSI
import Debug.Trace
import Control.Concurrent (threadDelay)
import Model.AST.Showable
import qualified Model.AST.PrettyPrint as PP

{-
Minek van state-je?

ExpInfo (State):
NoChange
Error
Done
Unsafe
Change ChangeType

ChangeType:
---  mi legyen? ---
1) Substitute --- behelyettesítés
2) ByDef      --- általános kiértékelés (def szerinti redukció)
3) Evaluating --- függvényredukció 
?) 

1)
x{0} + 1 => [3] + 1
{0}
x = 3

2)
1 + 1 + 2 => [2] + 2

3)
inc{0} 2 => a{a = 2} + 1
{0}
inc a = a + 1
------------------
GrhsState:      -- az aktuális állapotot színként reprezentáljuk a |-nál
GrhsOk -- zöld 
GrhsFail -- piros
GrhsUnk -- ???

PatInfo (State): 
mintaillesztéshez fogjuk majd használni, és jelezni, hogy illeszkedett-e

--- mi legyen? --- PMState??? esetleg kiegészítve?


------------------------------------------------------------

RhsState:
RhsOk | RhsFail | RhsUnk

StmtState:
-- ezt nem fogjuk kihasználni

DeclInfo (State):
-- ezt nem fojuk használni

-}

printIt :: Exp -> IO ()
printIt inp = do
    putStrLn "----------------------"
    let (exp, cache, _) = collectCaches (inp,[],ID 0)
    printAST 0 exp
    newLine
    newLine
    if null cache then putStrLn "" else putStrLn "Local bindings include:"
    -- print cache here
    let len = maximum $ map (length . show . fst) cache
    mapM_ (printCache2 len) (reverse cache)




class Printable a where
    printAST  :: Padding -> a -> IO ()



-- cél: nem kell scope, szín sem, csak mutassuk meg rendesen a decl-t



instance Printable Module where
    printAST pad (Module (ModuleName mn) _ _ ds) = do
        put $ "module " <> mn <> " " <> "where\n"
        mapM_ (\d -> printAST pad d >> newLine) ds--(filter isPatOrFun ds)


instance Printable (Maybe Name, Maybe EType) where
    printAST pad (Just n, Just ty) = out n >> put " :: " >> put (dropWhile (==' ') $ PP.prettyPrint ty) >> newLine
    printAST _   _ = return ()


-- A típusok a declaration-ökben vannak, ki lehetne akár írni
instance Printable Decl where
    printAST :: Padding -> Decl -> IO ()
    printAST pad d@(FunBind (DeclInfo st ty) ms) = printAST pad (getName d, ty) >> mapM_ (printAST pad) ms
    printAST pad (TypeSig n t) = padding pad >> mapM_ out n >> put " :: " >> out t >> newLine
    printAST pad d@(PatBind (DeclInfo _ ty) pat rhs bs) = do
        printAST pad (getName d, ty)
        padding pad
        out pat
        printAST pad (rhs,bs)
     
instance Printable Match where
    printAST pad (Match s n ps rhs bs) = do
        padding pad
        out n
        mapM_ (\x -> ws >> out x) ps
        printAST pad (rhs,bs)




instance Printable (Rhs, Maybe Binds) where
    printAST pad (rhs, bs) = do 
        case rhs of
            UnGuardedRhs s e    -> printAST (pad + 4) rhs
            GuardedRhss s [grhs] -> ws >> printAST (pad + 4) rhs
            GuardedRhss s grhss  -> do 
                newLine
                padding (pad + 4)
                printAST (pad + 4) rhs
        case bs of
            Nothing -> newLine
            Just bs -> do
                put " where "
                newLine
                printAST (pad + 4) bs









instance Printable Rhs where
    printAST pad (UnGuardedRhs s e) = ws >> put "= " >> printAST pad e
    printAST pad (GuardedRhss s (rhs:rhss)) = do
        printAST (pad + 4) rhs
        mapM_ (\rhs' -> newLine >> padding pad >> printAST (pad + 4) rhs') rhss


-- Önmagát padding nélkül írja ki
instance Printable GuardedRhs where
    printAST pad (GuardedRhs s (st:stmts) e) = do
        put "| "
        printAST pad st
        mapM_ (\stmt -> put ", " >> printAST pad stmt) stmts
        ws 
        put "= "
        printAST pad e
        
instance Printable Stmt where
    printAST pad (Qualifier s e) = printAST pad e

instance Printable Binds where
    printAST pad (BDecls bs) = mapM_ (\bind -> padding pad >> printAST pad bind) (filter isPatOrFun bs) >> newLine


instance Printable Exp where
    printAST pad e = case e of
        Var i _ -> printWithState (getS i) e
        Con i _ -> printWithState (getS i) e
        Lit i _ -> printWithState (getS i) e
        -- Infixapp with an operator that has scope
        InfixApp _ e1 qop e2 -> do
            printAST pad e1
            ws
            case qop of
                QVArOPP i id n -> printWithState (getS i) (QVarOp i n) >> putStr ("{" <> show id <> "}")
                QVarOp i n     -> printWithState (getS i) (QVarOp i n)
                QConOp i n     -> printWithState (getS i) (QVarOp i n)
            ws
            printAST pad e2
        App _ e1 e2 -> do
            printAST pad e1
            ws
            printAST pad e2
        Paren _ e' -> put "(" >> printAST pad e' >> put ")"
        NegApp i e' -> do
            getColor (getS i)
            put "-"
            printAST pad e'
        ExpTypeSig e ty -> printAST pad e >> put " :: " >> out ty
        Tuple _ exps -> do 
            put "("
            printWithSep ", " pad exps
            put ")"
        List _ exps -> do
            put "["
            printWithSep ", " pad exps
            put "]"
        Lambda i ps e' -> do -- Lambda ExpInfoSc [Pat] Exp
            put "\\"
            error $ "printable: add patterns"
            put " -> "
            printAST pad e' 
        Let i bs e -> undefined -- Let ExpInfoSc Binds Exp
        VarP i id qn -> do
            printAST pad (Var i qn)
            put $ "{" <> show i <> "}"

printWithSep :: Printable a => String -> Padding -> [a] -> IO ()
printWithSep sep pad [] = return ()
printWithSep sep pad [x] = printAST pad x
printWithSep sep pad (x:xs) = do
    printAST pad x
    put sep
    printWithSep sep pad xs


-- majd csak a state-eknél kell implementálni, addig prettyPrint van
--instance Printable Pat where

-- csak elemi exp-nél szabad használni
printWithState :: PP.PrettyPrint e => State -> e -> IO ()
printWithState s e = do
    getColor s
    put $ PP.prettyPrint e
    reset






    

{-
instance Printable Pat where
    printAST (PVar (PatInfo s _) n) = do
        putStr $ newPrint n
    printAST (PLit s _ l) = do
        putStr $ newPrint l
    printAST (PInfix s p1 n p2) = do
        printAST p1
        putStr " "
        putStr $ newPrint n
        putStr " "
        printAST p2
    printAST (PApp s n ps) = do
        putStr $ newPrint n
        putStr " "
        mapM_ printAST ps
    printAST (PTuple s ps) = do
        putStr "("
        mapM_ printAST ps
        putStr ")"
    printAST (PList s ps) = do
        putStr "["
        mapM_ printAST ps
        putStr "]"
    printAST (PParen p) = do
        putStr "("
        printAST p
        putStr ")"
    printAST (PWildCard _) = do
        putStr "_"
    printAST (PAsPat (PatInfo _ _) n p) = do
        putStr $ newPrint n
        putStr "@"
        printAST p
    printAST (PIrrPat p) = do
        putStr "~"
        printAST p
-}




------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
------------------------------------------------------------


printCache2 :: Int -> (ID, [Decl]) -> IO ()
printCache2 len (idv, decls) = do
    let ids = "{" <> show idv <> "} "
    let padding = len + 3
    putStrLn ids
    mapM_ (\d -> printAST padding d >> newLine) decls


isPatOrFun :: Decl -> Bool
isPatOrFun (PatBind {}) = True
isPatOrFun (FunBind {}) = True
isPatOrFun _ = False

out :: PP.PrettyPrint a => a -> IO ()
out = putStr . PP.prettyPrint
put = putStr

getName :: Decl -> Maybe Name
getName (FunBind _ (Match _ n _ _ _:_)) = Just n
getName (TypeSig _ _ ) = Nothing
getName (PatBind _ (PVar _ n) _ _) = Just n
getName _ = Nothing


ws = putStr " "
padding :: Int -> IO ()
padding i = putStr $ replicate i ' '


-- In seconds
sleep :: Float -> IO ()
sleep = threadDelay . round . (* 1000000)

-- Foreground | Background | Underlining
-- Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
-- Dull | Vivid


newLine = putStrLn ""
type Cache = [(ID, [Decl])]

--  Let ExpInfo Binds Exp
--  Tuple ExpInfo [Exp]
--  List ExpInfo [Exp]
--  ExpTypeSig Exp EType

next :: ID -> ID
next (ID i) = ID $ i + 1

collectCaches :: (Exp,Cache,ID) -> (Exp,Cache,ID)
collectCaches (exp,cache,id) = help id exp cache where
    help id   (Var s@(ExpInfoS _ _ (Just sc)) qn) cache = (VarP s id qn, (id, sc):cache, next id)
    help id e@(Var   (ExpInfoS _ _ Nothing) n) cache = (e, cache, id)
    help id (InfixApp s e1 op e2) cache = (InfixApp s e1' op' e2', cache''', id''') where
        (e1', cache', id') = help id e1 cache
        (op', cache'', id'') = labelOp (op, cache', id')
        (e2', cache''', id''') = help id'' e2 cache''
    help id (Paren s e) cache = (Paren s e', cache', id') where
        (e', cache', id') = help id e cache
    help id (App s e1 e2) cache = (App s e1' e2', cache'', id'') where
        (e1', cache', id') = help id e1 cache
        (e2', cache'', id'') = help id' e2 cache'
    help id (NegApp s e) cache = (NegApp s e', cache', id') where
        (e', cache', id') = help id e cache
    help id (Lambda s ps e) cache = (Lambda s ps e', cache', id') where
        (e', cache', id') = help id e cache
    help id e@(Lit s l) cache = (e, cache, id)
    help id e@(Con s n) cache = (e, cache, id)
    help id e@(List s es) cache = (List s es, c', id') where
        (es',c',id') = labelList (es,cache,id)
    help id e@(Tuple s es) cache = (Tuple s es, c', id') where
        (es',c',id') = labelList (es,cache,id)

    labelList :: ([Exp], Cache, ID) -> ([Exp], Cache, ID)
    labelList ([], c, id) = ([], c, id)
    labelList ((x:xs), c, id) = (x':xs', cc', id'') where
        (x',c',id') = help id x c
        (xs',cc',id'') = labelList (xs, c', id')

    labelOp :: (QOp,Cache,ID) -> (QOp,Cache,ID)
    labelOp (QVarOp s@(ExpInfoS st t (Just sc)) qn, cache, id) = (QVArOPP s id qn,(id, sc) : cache,next id)
    labelOp (QVarOp s@(ExpInfoS st t Nothing) qn, cache, id) = (QVarOp s qn, cache, id)
    labelOp (QConOp s qn, cache, id) = (QConOp s qn, cache, id)

type Padding = Int

--------------

reset     = setSGR [Reset]
white     = setSGR [SetColor Foreground Vivid White]
red       = setSGR [SetColor Foreground Vivid Red]
blue      = setSGR [SetColor Foreground Vivid Blue]
green     = setSGR [SetColor Foreground Vivid Green]
cyan      = setSGR [SetColor Foreground Vivid Cyan]
magenta   = setSGR [SetColor Foreground Vivid Magenta]
yellow    = setSGR [SetColor Foreground Vivid Yellow]

whiteD    = setSGR [SetColor Foreground Dull White]
redD      = setSGR [SetColor Foreground Dull Red]
blueD     = setSGR [SetColor Foreground Dull Blue]
greenD    = setSGR [SetColor Foreground Dull Green]
cyanD     = setSGR [SetColor Foreground Dull Cyan]
magentaD  = setSGR [SetColor Foreground Dull Magenta]
yellowD   = setSGR [SetColor Foreground Dull Yellow]
black     = setSGR [SetColor Foreground Vivid Black]

undefinedColor = magentaD >> setSGR [SetColor Background Dull White]

class Colorable a where
    getColor :: a -> IO ()

instance Colorable State where
    getColor NoChange = whiteD
    getColor (Error _) = red
    getColor UnSafe = magenta
    getColor Done = green
    getColor (Change changeType) = getColor changeType

instance Colorable ChangeType where
    getColor Substitute = undefinedColor
    getColor ByDef = undefinedColor
    getColor Evaluating = undefinedColor

instance Colorable GrhsState where
    getColor GrhsOk = green
    getColor GrhsFail = red
    getColor GrhsUnk = blue

