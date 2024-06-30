open Parser;;
open Lexer;;
open Calc;;

let lexbuf = Lexing.from_string "
append([], L, L).
append([X|R], L, [X|Z]) :- append(R, L, Z).

mapcons(X, [], []) :- !.
mapcons(X, [Y|R], [[X|Y] | Z]) :- mapcons(X, R, Z).

powerI([], [[]]) :- !.
powerI([X|R], P) :- powerI(R, P1), mapcons(X, P1, P2), append(P2, P1, P).

";;
let result = Parser.program Lexer.token lexbuf ;;

let lexbuff = Lexing.from_string "?powerI([2,3],X)." ;;

let querr = Parser.queryy Lexer.token lexbuff ;;


let () = eval(querr,result);;


(*
TEST CASE 1

fun(X) :- high(X), thin(X). func(X) :- anurag(X), boom(X). thin(vw_beatle). thin(ford_escort). 
boom(harley_davidson). high(vw_beatle). high(ford_escort). anurag(harley_davidson).

*)

(*
TEST CASE 2

bro(johnny,paulin). bro(paulin,tommy).
bro(tommy,maryline). sis(X,Y):- bro(X,Y). sis(X,Y):- bro(X,Z), bro(Z,Y)

*)

(*
TEST CASE 3

append([],L,L).
append([H|T],L,[H|R]) :- append(T,L,R).

?append([two,[5]],[1],Y).

*)

(*

TEST CASE 4

mem(X,[]):- fail. mem(X,[X|R]). mem(X,[R|H]) :- mem(X,H).

?mem(X,[3,5,raj,kk,ki]).

*)


(*

TEST CASE 5
parent(john, mary).
parent(john, alice).
parent(alex, peter).
parent(alex, david).
parent(mary, lily).
parent(mary, emma).
parent(alice, mia).
parent(alice, sophia).
parent(peter, oliver).
parent(peter, ethan).
parent(david, amelia).
parent(david, isabella).

male(john).
male(alex).
male(peter).
male(david).
male(oliver).
male(ethan).

female(mary).
female(alice).
female(lily).
female(emma).
female(mia).
female(sophia).
female(amelia).
female(isabella).

grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).

sibling(X, Y) :-
    parent(P, X),
    parent(P, Y).

aunt(Aunt, Person) :-
    female(Aunt),
    sibling(P, Person),
    parent(P, Person),
    parent(Aunt, P).

uncle(Uncle, Person) :-
    male(Uncle),
    sibling(P, Person),
    parent(P, Person),
    parent(Uncle, P).

cousin(X, Y) :-
    parent(PX, X),
    parent(PY, Y),
    sibling(PX, PY).
*)

(*

TEST CASE 6

del(X, [X|L], L).
del(X, [Y|L], [Y|L1]) :- del(X, L, L1).

?del(3,[3,5,7,3],X).

*)

(* TEST CASE 7

append([], L, L).
append([X|R], L, [X|Z]) :- append(R, L, Z).

mapcons(X, [], []) :- !.
mapcons(X, [Y|R], [[X|Y] | Z]) :- mapcons(X, R, Z).

powerI([], [[]]) :- !.
powerI([X|R], P) :- powerI(R, P1), mapcons(X, P1, P2), append(P2, P1, P).

?powerI([2,3],X).

*)