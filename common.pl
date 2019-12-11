:- module(common, [
              lines/2, split_string_on/3,

              % Array operations
              mk_array/2, mk_empty_array/2, array_set/3, array_get/3, array_get/4,
	      array_get_offset/4, array_set_offset/4, array_get_offset/5
          ]).

:- use_module(library(clpfd)).


split_string_on(Str, SplitOn, Parts) :- split_string(Str, SplitOn, SplitOn, Parts).


lines(Str, Lines) :- split_string_on(Str, "\n", Lines).


% Array operations on 0-based, fixed-length arrays

% create array from list
mk_array(Members, Array) :- compound_name_arguments(Array, '$array$', Members).


% create array of non-unified variables
mk_empty_array(Len, Array) :-
	length(Lst, Len),
	mk_array(Lst, Array).


% set at position
array_set(Array, Pos, Val) :-
	nonvar(Val),
	Pos1 is Pos+ 1,
	nb_setarg(Pos1, Array, Val).


% get from position, fail if no value present (non-unified variable)
array_get(Array, Pos, Val) :-
	Pos1 is Pos + 1,
	arg(Pos1, Array, ArrayVal),
	nonvar(ArrayVal),
	Val = ArrayVal.


% get from position, return default if no value present
array_get(Array, _,  Pos, Val) :- array_get(Array, Pos, Val), !.
array_get(_, Default, _, Default).


% array get helper for Pos + Offset access
array_get_offset(Array, Pos, Offset, Val) :-
    Pos1 #= Pos + Offset,
    array_get(Array, Pos1, Val).


% array set helper for Pos + Offset access
array_set_offset(Array, Pos, Offset, Val) :-
    Pos1 #= Pos + Offset,
    array_set(Array, Pos1, Val).


% array get helper for Pos + Offset access with default
array_get_offset(Array, Default, Pos, Offset, Res) :-
    Pos1 #= Pos + Offset,
    array_get(Array, Default, Pos1, Res).

















