:- module(day02, [solve/2]).

:- use_module(library(clpfd)).
:- use_module(common, [split_string_on/3, array_get/3,
                       array_get_offset/4, array_set/3, mk_array/2]).

% helper, which reads address of argument, and gets value from that
% address
array_read_arg(Array, Pos, Offset, Val) :-
    array_get_offset(Array, Pos, Offset, ValPos),
    array_get(Array, ValPos, Val).


% dispatch opcodes for position
run_helper(99, _, _) :- !.
run_helper(OpCode, Array, Pos) :-
    array_read_arg(Array, Pos, 1, Arg1),
    array_read_arg(Array, Pos, 2, Arg2),
    array_get_offset(Array, Pos, 3, Dest),
    interp(OpCode, Arg1, Arg2, Res),
    array_set(Array, Dest, Res),
    Pos1 #= Pos+4,
    run(Array, Pos1).


% run until the end
run(Array, Pos) :-
   array_get(Array, Pos, OpCode),
   run_helper(OpCode, Array, Pos).


% implementation of opcodes
interp(1, A, B, Res) :- Res #= A + B.
interp(2, A, B, Res) :- Res #= A * B.


% obtain solution for a pair of replacements
solve_helper(Array, Repl1, Repl2, Res) :-
    duplicate_term(Array, Array1), % we use mutable updates => create our own copy
    array_set(Array1, 1, Repl1),
    array_set(Array1, 2, Repl2),
    run(Array1, 0),
    array_get(Array1, 0, Res).


% find solution for second part
solve2(Array, Res) :-
    between(0, 99, Noun), % generate noun ...
    between(0, 99, Verb), % .. and verb
    solve_helper(Array, Noun, Verb, ResRun), % run solution
    ResRun = 19690720, % check result
    !, % commit to soultion found
    Res #= (100 * Noun) + Verb.


solve(R1, R2) :-
    input(Inp),
    split_string_on(Inp, ",", NumStrs),
    maplist(number_string, Numbers, NumStrs),
    mk_array(Numbers, Array),
    solve_helper(Array, 12, 2, R1),
    solve2(Array, R2).


input("1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,9,19,23,2,23,13,27,1,27,9,31,2,31,6,35,1,5,35,39,1,10,39,43,2,43,6,47,1,10,47,51,2,6,51,55,1,5,55,59,1,59,9,63,1,13,63,67,2,6,67,71,1,5,71,75,2,6,75,79,2,79,6,83,1,13,83,87,1,9,87,91,1,9,91,95,1,5,95,99,1,5,99,103,2,13,103,107,1,6,107,111,1,9,111,115,2,6,115,119,1,13,119,123,1,123,6,127,1,127,5,131,2,10,131,135,2,135,10,139,1,13,139,143,1,10,143,147,1,2,147,151,1,6,151,0,99,2,14,0,0").












