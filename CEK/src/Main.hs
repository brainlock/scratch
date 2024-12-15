module Main where

import Data.Map (Map)
import qualified Data.Map as M

type Var = String

data Lambda = Var :-> Exp
    deriving (Show, Eq)

data LetBinding = Var := Exp
    deriving (Show, Eq)

data Exp =
    Const Int
    | Bool Bool
    | Ref Var
    | Lam Lambda
    | Exp :@ Exp
    | Let (LetBinding, Exp)
    | If Exp (Exp, Exp)
    deriving (Show, Eq)

data Value =
    Vconst Int
    | Vbool Bool
    | Vclosure Lambda Env
    deriving (Show, Eq)

data Cont =
    CtEmpty
    | CtArg (Exp, Env, Cont)
    | CtFn (Lambda, Env, Cont)
    | CtIf ((Exp, Exp), Env, Cont)
    deriving (Show, Eq)

type Env = Map Var Value
type State = (Exp, Env, Cont)

initialState :: Exp -> State
initialState prg = (prg, M.empty, CtEmpty)

isFinal :: State -> Bool
isFinal (Const _, _, CtEmpty) = True
isFinal (Bool _, _, CtEmpty) = True
isFinal (_, _, _) = False

envLookup :: Env -> Var -> Value
envLookup env name = case M.lookup name env of
    Nothing -> error $ "err: no binding for " ++ name
    Just v -> v

step :: State -> State

step s@(Const _, _, CtEmpty) = s

step (Ref name, env, ct) = case envLookup env name of
    Vconst x -> (Const x, env, ct)
    Vclosure lam closureEnv -> (Lam lam, closureEnv, ct)
    Vbool x -> (Bool x, env, ct)

step (f :@ e, env, ct) = (f, env, CtArg (e, env, ct))

step (Lam f, env, CtArg (argExp, argEnv, argCt)) =
    (argExp, argEnv, CtFn (f, env, argCt))

step (Const k, _, CtFn (x :-> b, fnEnv, fnCt)) = (b, M.insert x (Vconst k) fnEnv, fnCt)
step (Bool val, _, CtFn (x :-> b, fnEnv, fnCt)) = (b, M.insert x (Vbool val) fnEnv, fnCt)
step (Lam f, env, CtFn (x :-> b, fnEnv, fnCt)) = (b, M.insert x (Vclosure f env) fnEnv, fnCt)

step (Let (x := val, b), env, cont) = (Lam (x :-> b) :@ val, env, cont)

step (If condExp (thenExp, elseExp), env, cont) =
    (condExp, env, CtIf ((thenExp, elseExp), env, cont))
step (Bool True, _, CtIf ((thenExp, _), env', cont)) = (thenExp, env', cont)
step (Bool False, _, CtIf ((_, elseExp), env', cont)) = (elseExp, env', cont)
step (e, _, CtIf (_, _, _)) =
    error $ "type error: if condition must evaluate to bool, got: " ++ show e

step s = error $ "invalid state: " ++ show s


eval :: Exp -> (Exp, [State])
eval prg = eval' (initialState prg) []
    where
        eval' state@(res, _, _) trace =
            if isFinal state
            then (res, state:trace)
            else eval' (step state) (state:trace)

test :: String -> Exp -> Exp -> IO ()
test name expectedResult prg = do
    let (res, _) = eval prg
    putStr $ if res == expectedResult then "✅ " else "❌ "
    putStrLn name

example1 :: Exp
example1 = Lam ("x" :-> Const 42) :@ Const 43

main :: IO ()
main = do
    test "const" (Const 42) $ Const 42
    test "f const" (Const 42) $ Lam ("x" :-> Const 42) :@ Const 43
    test "f id" (Const 42) $ Lam ("x" :-> Ref "x") :@ Const 42
    test "f as arg" (Const 42) $ Lam ("f" :-> Lam ("x" :-> (Ref "f" :@ Ref "x"))) :@ Lam ("x" :-> Ref "x") :@ Const 42
    test "scope 1" (Const 43) $ Lam ("x" :-> Lam ("y" :-> Ref "y")) :@ Const 42 :@ Const 43
    test "scope 2" (Const 42) $ Lam ("x" :-> Lam ("y" :-> Ref "x")) :@ Const 42 :@ Const 43
    test "let" (Const 42) $ Let ("x" := Const 42, Lam ("y" :-> Ref "x") :@ Const 43)
    test "let f" (Const 42) $ Let ("f" := Lam ("x" :-> Ref "x"), Ref "f" :@ Const 42)
    test "bool val" (Bool True) $ Lam ("x" :-> Bool True) :@ Bool False
    test "if 1" (Const 42) $ Lam ("x" :-> Lam ("y" :->
            If (Bool True) (Ref "x", Ref "y")
        ))
        :@ Const 42 :@ Const 43
    test "if 2" (Const 43) $ Lam ("x" :-> Lam ("y" :->
            If (Bool False) (Ref "x", Ref "y")
        ))
        :@ Const 42 :@ Const 43
    test "if 3" (Const 42) $ Lam ("x" :-> Lam ("y" :-> Lam ("cond" :->
            If (Ref "cond") (Ref "x", Ref "y")
        )))
        :@ Const 42 :@ Const 43 :@ Bool True
    test "if 4" (Const 43) $ Lam ("x" :-> Lam ("y" :-> Lam ("cond" :->
            If (Ref "cond") (Ref "x", Ref "y")
        )))
        :@ Const 42 :@ Const 43 :@ Bool False
    test "if 5" (Const 42) $ Let ("x" := Const 43, Let ("y" := Const 44, Let ("f" := Lam ("x" :-> Bool True),
            If (Ref "f" :@ Const 123) (Const 42, Const 54)
        )))
