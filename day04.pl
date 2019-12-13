:- module(day04, [solve/2]).

parse_input(Input, Low, High) :- phrase((number(Low), "-", number(High)), Input).


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
enum_valid(Low, High, Constraint, LN) :-
    between(Low, High, N),
    number_digits(N, LN),
    non_decreasing(LN),
    Goal =.. [Constraint, LN],
    call(Goal).


% helper
solve_(Low, High, Constraint, Res) :-
    findall(LN, enum_valid(Low, High, Constraint, LN), LNs),
    length(LNs, Res).


solve(R1, R2) :-
    input(I), parse_input(I, Low, High),
    solve_(Low, High, constrain1, R1),
    solve_(Low, High, constrain2, R2).


input(`372304-847060`).
