module MiniLogo where
import Data.List

type Macro = String
type Var = String

-- 1 Define the abstract syntax of MiniLogo as a set of Haskell data types. You
-- should use built-in types for num, var, and macro. (If you want to define a
-- type Num, you will have to hide that name from the Prelude).

data Mode = Up
          | Down
          deriving (Show, Eq)

data Expr = Ref Var
          | Number Int
          | Add Expr Expr
          deriving (Show, Eq)

type Prog = [Cmd]

data Cmd = Pen Mode
         | Move (Expr, Expr)
         | Define Macro [Var] Prog
         | Call Macro [Expr]
         deriving (Show, Eq)

-- 2 Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on
-- the canvas) draws a line segment from (x1,y1) to (x2,y2).

-- Write the macro in MiniLogo concrete syntax (i.e. the notation defined by
-- the grammar and used in the example programs above). Include this definition
-- in a comment in your submission.
-- 
-- define line (x1, y1, x2, y2) {
--     pen up;
--     move (x1, y1);
--     pen down;
--     move (x2, y2);
--     pen up;
-- }

-- Encode the macro definition as a Haskell value using the data types defined
-- in Task 1. This corresponds to the abstract syntax of MiniLogo. Your
-- definition should look something like line = Define "line" ...

line = Define "line" ["x1", "y1", "x2", "y2"] [Pen Up, Move (Ref "x1", Ref "y1"), Pen Down, Move (Ref "x2", Ref "y2"), Pen Up]

-- 3 Use the line macro you just defined to define a new MiniLogo macro
-- nix (x,y,w,h) that draws a big “X” of width w and height h, starting from
-- position (x,y). Your definition should not contain any move commands.

-- Write the macro in MiniLogo concrete syntax and include this definition in a
-- comment in your submission.
-- define nix (x, y, w, h) {
--   line(x, y, x + w, y + h);
--   line(x + w, y, x, y + h);
-- }

-- Encode the macro definition as a Haskell value, representing the abstract
-- syntax of the definition.
nix = Define "nix" ["x", "y", "w", "h"] [
  Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")],
  Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Ref "y") (Ref "h")]]

-- 4 Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo
-- program that draws a staircase of n steps starting from (0,0). Below is a
-- visual illustration of what the generated program should draw for a couple
-- different applications of steps.
steps :: Int -> Prog
steps 0 = []
steps i = [Call "line" [Number i, Number i, Number (i - 1), Number i], Call "line" [Number i, Number i, Number i, Number (i - 1)]] ++ steps (i-1)

-- 5 Define a Haskell function macros :: Prog -> [Macro] that returns a list of
-- the names of all of the macros that are defined anywhere in a given MiniLogo
-- program. Don’t worry about duplicates—if a macro is defined more than once,
-- the resulting list may include multiple copies of its name.

macros :: Prog -> [Macro]
macros [] = []
macros (p:ps) = case p of
  Define m _ _ -> m:macros ps
  otherwise -> macros ps

-- 6 Define a Haskell function pretty :: Prog -> String that pretty-prints a
-- MiniLogo program. That is, it transforms the abstract syntax (a Haskell
-- value) into nicely formatted concrete syntax (a string of characters). Your
-- pretty-printed program should look similar to the example programs given
-- above; however, for simplicity you will probably want to print just one
-- command per line.

pretty :: Prog -> String
pretty [] = ""
pretty (Pen Up:xs) = "pen up; " ++ pretty xs
pretty (Pen Down:xs) = "pen up; " ++ pretty xs
pretty (Move (l, r):xs) = "move (" ++ prettyExpr l ++ ", " ++ prettyExpr r ++ "); " ++ pretty xs
pretty (Call n vs:xs) = n ++ "(" ++ intercalate ", " (map prettyExpr vs) ++ "); " ++ pretty xs
pretty (Define m vs p:ps) = "define " ++ m ++ "(" ++ intercalate ", " vs ++ ") {" ++ pretty p ++ "}; " ++ pretty ps

-- A helper function for pretty which pretty prints expressions
prettyExpr :: Expr -> String
prettyExpr (Number n) = show n
prettyExpr (Ref s) = s
prettyExpr (Add l r) = prettyExpr l ++ " + " ++ prettyExpr r

-- | 7 Define a Haskell function optE :: Expr -> Expr that partially evaluates
-- | expressions by replacing any additions of literals with the result. For
-- | example, given the expression (2+3)+x, optE should return the expression
-- | 5+x.
--   >>> optE (Add (Number 1) (Number 2))
--   Number 3
--   >>> optE (Add (Number 2) (Add (Ref "a") (Number 2)))
--   Add (Number 2) (Add (Ref "a") (Number 2))
optE :: Expr -> Expr
optE (Add (Number l) (Number r)) = Number $ l + r
optE otherwise = otherwise

-- | 8 Define a Haskell function optP :: Prog -> Prog that optimizes all of the
-- | expressions contained in a given program using optE.
--   >>> let a = Define "nix" ["x", "y", "w", "h"] [Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Number 1) (Ref "h")],Call "line" [Add (Ref "x") (Ref "w"), Ref "y", Ref "x", Add (Number 2) (Number 1)]]
--   >>> optP [a]
--   [Define "nix" ["x","y","w","h"] [Call "line" [Ref "x",Ref "y",Add (Ref "x") (Ref "w"),Add (Number 1) (Ref "h")],Call "line" [Add (Ref "x") (Ref "w"),Ref "y",Ref "x",Number 3]]]
optP :: Prog -> Prog
optP [] = []
optP (p:ps) = case p of
  Move (l, r) -> Move (optE l, optE r):optP ps
  Call m es -> Call m (map optE es):optP ps
  Define m vs p -> Define m vs (optP p):optP ps
  otherwise -> p:optP ps
