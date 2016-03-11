module HW3 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--     functions for generating MiniMiniLogo programs. It contains the type
--     definitions for Mode, Cmd, and Prog.
--   * Render.hs contains code for rendering the output of a MiniMiniLogo
--     program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Down) (s, p)    = ((Down, p), Nothing)
cmd (Pen Up) (s, p)      = ((Up, p), Nothing)
cmd (Move i j) (Up, p)   = ((Up, (i,j)), Nothing)
cmd (Move i j) (Down, p) = ((Down, (i,j)), Just (p, (i,j)))


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] s = (s, [])
prog (l:ls) s = case cmd l s of
    (ns, Just ml) -> (\(s, ls) -> (s, ml:ls)) $ prog ls ns
    (ns, Nothing) -> prog ls ns


--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
--   The Y Combinator, the lambda calculus function which will return a
--   fixed-point for any function it is given. It is the key which opens the
--   pandora's box of recursion
--   Y := λg.(λx.g (x x)) (λx.g (x x))
--   https://en.wikipedia.org/wiki/Lambda_calculus#Recursion_and_fixed_points
--   https://medium.com/@ayanonagon/the-y-combinator-no-not-that-one-7268d8d9c46
--   It's only theoretically pretty.
amazing :: Prog
amazing = (lambda 10 40 ++ gee 16 40 ++ box 20 28 ++ openParen 24 40 ++
  lambda 25 40 ++ nix 32 28 6 12 ++ box 40 28 ++ gee 46 40 ++ openParen 52 40 ++
  nix 56 28 6 12 ++ nix 65 28 6 12 ++ closeParen 72 40 ++ closeParen 74 40 ++
  openParen 10 20 ++ lambda 13 20 ++ nix 20 8 6 12 ++ box 28 8 ++ gee 31 20 ++
  openParen 38 20 ++ nix 40 8 6 12 ++ nix 48 8 6 12 ++ closeParen 56 20 ++
  closeParen 60 20)
--amazing = (lambda 10 20 ++ gee 16 20 ++ box 20 8 ++ openParen 24 20 ++ nix 24 8 6 12

gee :: Int -> Int -> Prog
gee x y = [Pen Up, Move x (y-6), Pen Down, Move (x+3) (y-6), Move (x+3) (y-14),
  Move (x) (y-14), Pen Up, Move x (y-6), Pen Down, Move x (y-9), Move (x+3) (y-9)]

lambda :: Int -> Int -> Prog
lambda x y = [Pen Up, Move x y, Pen Down, Move (x+6) (y-12), Pen Up,
  Move (x) (y-12), Pen Down, Move (x+2) (y-4), Pen Up]

openParen :: Int -> Int -> Prog
openParen x y = [Pen Up, Move x y, Pen Down, Move (x-2) (y-6), Move (x) (y-12), Pen Up]



closeParen :: Int -> Int -> Prog
closeParen x y = [Pen Up, Move x y, Pen Down, Move (x+2) (y-6), Move (x) (y-12), Pen Up]
