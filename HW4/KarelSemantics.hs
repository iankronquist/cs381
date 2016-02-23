module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not $ test t w r
test (Facing d) w (p, c, i) = c == d
test (Clear d) w (p, c, i) = isClear (neighbor (cardTurn d c) p) w
test Beeper w (p, c, i) = hasBeeper p w
test Empty w r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move d w (p, c, i) = let np = neighbor c p
                          in if isClear np w
                                then OK w (setPos np (p, c,i))
                                else Error ("Blocked at: " ++ show np)
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r
                        in if not $ isEmpty r
                              then OK (incBeeper p w) (decBag r)
                              else Error "No beeper to put."
stmt (Turn d) _ w (p, c, i) = OK w $ setFacing (cardTurn d c) (p, c,i)
stmt (Call m) d w r = case lookup m d of
                        Nothing -> Error ("Undefined macro: " ++ m)
                        Just s  -> stmt s d w r
stmt (Iterate 0 _) _ w r = OK w r
stmt (Iterate n s) d w r = case stmt s d w r of
                            OK w' r'  -> stmt (Iterate (n-1) s) d w' r'
                            otherwise -> otherwise
stmt (If t f s) d w r = if test t w r then stmt f d w r else stmt s d w r
stmt (Block []) d w r = OK w r
stmt (Block (s:ss)) d w r = case stmt s d w r of
                              OK w' r' -> stmt (Block ss) d w' r'
                              otherwise -> otherwise
stmt (While t s) d w r = if test t w r then case stmt s d w r of
                              OK w' r' -> stmt (While t s) d w' r'
                              otherwise -> otherwise
                            else OK w r
--stmt _ _ _ _ = undefined
    
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) = stmt s m
