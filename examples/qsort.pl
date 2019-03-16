

% Some peano arithmetic

nat(z).
nat(s(X)) :- nat(X).

nateq(z, z).
nateq(s(X), s(Y)) :- nateq(X, Y).

add(z, X, X).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

lt(z, s(_)).
lt(s(X), s(Y)) :- lt(X, Y).

lte(X, Y) :- nateq(X, Y).
lte(X, Y) :- lt(X, Y).

gt(s(_), z).
gt(s(X), s(Y)) :- gt(X, Y).

gte(X, Y) :- nateq(X, Y).
gte(X, Y) :- gt(X, Y).

% ---

% Usage:
% ?- quicksort([s(s(z)), s(z), z, s(s(s(s(z))))], X).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs,Ys,Zs).

quicksort([Pivot|Rest], Sorted) :-
  partition(Rest, Pivot, Left, Right),
  quicksort(Left, SortedLeft),
  quicksort(Right, SortedRight),
  append(SortedLeft, [Pivot|SortedRight], Sorted).
quicksort([], []).

partition([X|Xs], Y, [X|Ls], Rs) :-
  lte(X, Y), partition(Xs, Y, Ls, Rs).
partition([X|Xs], Y, Ls, [X|Rs]) :-
  gt(X, Y), partition(Xs, Y, Ls, Rs).
partition([], _, [], []).

% % Same in OCaml:
% let rec qsort = function
%    | [] -> []
%    | pivot :: rest ->
%        let is_less x = x < pivot in
%        let left, right = List.partition is_less rest in
%        qsort left @ (pivot :: qsort right)
