
append([], A, A).
append([H|T], L, [H|R1]) :- append(T, L, R1).

member(A, [A|_]).
member(A, [_|Rest]) :- member(A, Rest).
