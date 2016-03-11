% Here are a bunch of facts describing the Simpson's family tree.
% Don't change them!

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

married_(abe,mona).
married_(clancy,jackie).
married_(homer,marge).

married(X,Y) :- married_(X,Y).
married(X,Y) :- married_(Y,X).

parent(abe,herb).
parent(abe,homer).
parent(mona,homer).

parent(clancy,marge).
parent(jackie,marge).
parent(clancy,patty).
parent(jackie,patty).
parent(clancy,selma).
parent(jackie,selma).

parent(homer,bart).
parent(marge,bart).
parent(homer,lisa).
parent(marge,lisa).
parent(homer,maggie).
parent(marge,maggie).

parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
child(X,Y) :- parent(Y,X).



% 2. Define two predicates `isMother/1` and `isFather/1`.
isMother(X) :- female(X), parent(X,_).
isFather(X) :- male(X), parent(X,_).


% 3. Define a predicate `grandparent/2`.
grandparent(X,Z) :- parent(X,Y),parent(Y,Z).


% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
sibling(X,Y) :- parent(S,X), parent(S,Y), X \= Y.


% 5. Define two predicates `brother/2` and `sister/2`.
brother(X,Y) :- male(X), sibling(X,Y).
sister(X,Y) :- female(X), sibling(X,Y).


% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
siblingInLaw(X,Y) :- sibling(Y,Z), married(X,Z).


% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
aunt(X,Y) :- female(X), sibling(X,Z), parent(Z,Y).
aunt(X,Y) :- female(X), married(X,Z), sibling(Z,A), parent(A,Y).
uncle(X,Y) :- male(X), sibling(X,Z), parent(Z,Y).
uncle(X,Y) :- male(X), married(X,Z), sibling(Z,A), parent(A,Y).


% 8. Define the predicate `cousin/2`.
cousin(X,Y) :- parent(XP,X),parent(YP,Y),sibling(XP,YP).


% 9. Define the predicate `ancestor/2`.
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y), ancestor(X,Z).


% Extra credit: Define the predicate `related/2`.
related(X,Y) :- ancestor(X,Y).
related(X,Y) :- ancestor(Y,X).
related(X,Y) :- aunt(X,Y).
related(X,Y) :- aunt(Y,X).
related(X,Y) :- uncle(X,Y).
related(X,Y) :- uncle(Y,X).
related(X,Y) :- cousin(X,Y).
related(X,Y) :- siblingInLaw(X,Y).
related(X,Y) :- sibling(X,Y).



%%
% Part 2. Language implementation (see course web page)
%%
cmd(add,[F,S|T],S2) :- X is (F+S), S2 = [X|T].
cmd(lte,[F,S|T], S2) :- X = (F =< S ->Y=t;Y=f), call(X), S2 = [Y|T].
cmd(if(R,_),[t|T], S2) :- prog(R,T,S2).
cmd(if(_,W),[f|T], S2) :- prog(W,T,S2).
cmd(X,T,S2) :- S2 = [X|T].

prog([], S1, S2) :- S2 = S1.
prog([C|T], S1, S2) :- cmd(C, S1, S3), prog(T,S3,S2).
