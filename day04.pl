:- module(day04, [solve/2]).

:- use_module(library(apply)).

parse_input(Input, Low, High) :- phrase((number(Low), "-", number(High)), Input).


% using CLP/FD for this gives attrocious performance (281s vs 2.7s)
number_digits(N, num(D1, D2, D3, D4, D5, D6)) :-
    divmod(N,  100000, D1, N1),
    divmod(N1, 10000,  D2, N2),
    divmod(N2, 1000,   D3, N3),
    divmod(N3, 100,    D4, N4),
    divmod(N4, 10,     D5, D6).


non_decreasing(num(D1, D2, D3, D4, D5, D6)) :- D1 =< D2, D2 =< D3, D3 =< D4, D4 =< D5, D5 =< D6.


% at least two neighbors match
constrain1(num(D1, D2, D3, D4, D5, D6)) :- once((D1 = D2; D2 = D3; D3 = D4; D4 = D5; D5 = D6)).


% excatly two neighbors match
constrain2(num(D1, D2, D3, D4, D5, D6)) :-
    once(
        (             D1 = D2, D2 \= D3
        ;   D1 \= D2, D2 = D3, D3 \= D4
        ;   D2 \= D3, D3 = D4, D4 \= D5
        ;   D3 \= D4, D4 = D5, D5 \= D6
        ;   D4 \= D5, D5 = D6
        )
    ).


% generates all matching numbers on backtracking
enum_valid(Low, High, (R1, R2)) :-
    between(Low, High, N),
    number_digits(N, LN), % generating the number ...
    non_decreasing(LN), % ... and checking non_decreasing only once ...
    ( constrain1(LN) -> R1 = 1 ; R1 = 0), % ... and checking both contraints ...
    ( constrain2(LN) -> R2 = 1 ; R2 = 0). % ... in one pass saves a lot of work


% helper
solve_(Low, High, R1, R2) :-
    findall(Res, enum_valid(Low, High, Res), Results),
    foldl(
        [(A1,A2), (N1, N2), (S1, S2)] >> (S1 is A1 + N1, S2 is A2 + N2),
        Results, (0,0), (R1, R2) % again, we only have to walk the list once
    ).


solve(R1, R2) :-
    input(I), parse_input(I, Low, High),
    solve_(Low, High, R1, R2).


input(`372304-847060`).
