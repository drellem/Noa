{- Translates the AST into a stateless intermediate representation (hence Functional IR).
This code doesn't actually work yet.

author:drellem
email:gate46dmiller@gmail.com

-}

module FIR(fir) where
import qualified Main
data Expr = Id String | Num Integer | Cons [Expr] | NType Main.NoaType | Nil deriving (Show)

fir :: [Main.Stmt]->Expr
fir [] = Nil


firStmt :: Main.Stmt->Expr
firStmt (Main.Assign s e t) = Cons [(Id "lambda"), (Id "~World"), Cons [(Id "record-set"), (Id "~World"), (Id s), (NType t), Cons [(firExpr e), (Id "~World")]]]
--(lambda ~World (record-set ~World s, t, (expr ~World)))
firStmt (Main.Foreach e l) = Cons [(Id "foreach"), firExpr e, fir l] --(lambda ~World (foreach (expr1 ~World) (expr2 ~World)))
firStmt (Main.FuncStmt s l) = Cons [(Id "lambda"), (Id "~World"), map l] --(lambda ~World (s (expr ~World) ~World))
firStmt (Main.If e l1 l2) = Cons [(Id "if"), fir l1, fir l2] --(lambda ~World (if (expr1 ~World) (expr2 ~World)))
--Even nested expressions should be lambda which takes and returns world
firExpr :: Main.Expr->Expr
firExpr (Main.Id s) = Cons [(Id "record-get"), (Id "~World"), (Id s)]
firExpr (Main.Num n) = Num n
firExpr (Main.Range i j) = Cons [(Id "range"), (Num i), (Num j)]
firExpr (Main.FuncExpr s l) = Cons $ [(Id s)]++ (map firExpr l)
firExpr (Main.FuncLit [] stmts) = Cons [(Id "lambda"), (Id "~World"), fir stmts]
firExpr (Main.FuncLit (x:xs) stmts) = Cons [(Id "lambda"), (Id x), firExpr( Main.FuncLit xs stmts)]
