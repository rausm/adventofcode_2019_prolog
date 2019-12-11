:- module(day03, [solve/2]).

:- use_module(library(clpfd)).
:- use_module(common, [lines/2, split_string_on/3]).

:- dynamic(w1/1).
:- dynamic(w2/1).
:- retractall(w1(_)).
:- retractall(w2(_)).

% parse line of instructions
instrs(Str, Instrs) :- split_string_on(Str, ",", Instrs).


% position after N steps in a direction
diff("U", N, (X,Y), (X,Y1)) :- Y1 #= Y + N.
diff("D", N, (X,Y), (X,Y1)) :- Y1 #= Y - N.
diff("R", N, (X,Y), (X1,Y)) :- X1 #= X + N.
diff("L", N, (X,Y), (X1,Y)) :- X1 #= X - N.


% interpret instruction as move from Pos to NewPos
interp_instr(Instr, Pos, NewPos) :-
    sub_string(Instr, 0, 1, _, Direction),
    sub_string(Instr, 1, _, 0, StepsStr),
    number_string(Steps, StepsStr),
    diff(Direction, Steps, Pos, NewPos).


% assert one point ...
assert_point(_, 0, 0) :- !. % ... unless it's (0,0)  ...
assert_point(Name, X, Y) :-
    Term =.. [Name, (X, Y)],
    (   (   \+ call(Term) % ... and unless it's been asserted (corners)
        ,   assertz(Term)
        )
    ;   true
    ).


% assert points on horizontal line
assert_points_x(Name, X, Y1, Y2) :-
    gen_range(Y1, Y2, Ys),
    forall(
        member(Y, Ys),
        assert_point(Name, X, Y)
    ).


% assert points on vertical line
assert_points_y(Name, Y, X1, X2) :-
    gen_range(X1, X2, Xs),
    forall(
        member(X, Xs),
        assert_point(Name, X, Y)
    ).


% between doesn't go downwards, this does (we need to assert the points
% in the right order !).
gen_range(N1, N2, Res) :-
    N2 #< N1,
    !,
    gen_range(N2, N1, Reversed),
    reverse(Reversed, Res).

gen_range(N1, N2, Res) :-
    findall(N, between(N1, N2, N), Res).


% assert 1 segment on the wire, depending on direction
assert_points(Name, (X, Y1), (X, Y2)) :- assert_points_x(Name, X, Y1, Y2).
assert_points(Name, (X1, Y), (X2, Y)) :- assert_points_y(Name, Y, X1, X2).


% assert 1 segment of wire
assert_wire_segment(Name, Instr, Pos, NewPos) :-
    interp_instr(Instr, Pos, NewPos),
    assert_points(Name, Pos, NewPos).


% helper, asserts all segments of wire
assert_wire(_, [], _) :- !.
assert_wire(Name, [Instr | Rest], Pos) :-
    assert_wire_segment(Name, Instr, Pos, NewPos),
    assert_wire(Name, Rest, NewPos),
    !.


% assert all points on wire
assert_wire(Name, Instrs) :-
    assert_wire(Name, Instrs, (0,0)).


% manhattan distance
dist((X,Y), Dist) :- Dist is abs(X) + abs(Y).


% all intersections of wires
intersections(Inters) :- findall(Pos, (w1(Pos), w2(Pos)), Inters).


% find solution for part 1
solve1(Intersections, Res) :-
    maplist(dist, Intersections, Distances),
    min_list(Distances, Res).


% number of steps up to Pos
length_up_to([Pos| _], Pos, Acc,  Acc) :- !.
length_up_to([_ | Rest], Pos, Acc, Res) :-
    Acc1 #= Acc + 1,
    length_up_to(Rest, Pos, Acc1, Res).


% solve intersection for part 2
solve2intersection(W1List, W2List, Pos, Res) :-
    length_up_to([(0,0) | W1List], Pos, 0, Steps1),
    length_up_to([(0,0) | W2List], Pos, 0, Steps2),
    Res is Steps1+Steps2.


% find shortest solution for part 2
solve2(Intersections, Res) :-
    findall(Pos, w1(Pos), W1List),
    findall(Pos, w2(Pos), W2List),
    maplist(solve2intersection(W1List, W2List), Intersections, Distances),
    min_list(Distances, Res).


solve(R1, R2) :-
    input(I),
    lines(I, [W1Str, W2Str]),
    instrs(W1Str, W1Instrs),
    instrs(W2Str, W2Instrs),
    assert_wire(w1, W1Instrs),
    assert_wire(w2, W2Instrs),
    intersections(Intersections),
    solve1(Intersections, R1),
    solve2(Intersections, R2).


input("R1009,U263,L517,U449,L805,D78,L798,D883,L777,D562,R652,D348,R999,D767,L959,U493,R59,D994,L225,D226,R634,D200,R953,U343,L388,U158,R943,U544,L809,D785,R618,U499,L476,U600,L452,D693,L696,U764,L927,D346,L863,D458,L789,U268,R586,U884,L658,D371,L910,U178,R524,U169,R973,D326,R483,U233,R26,U807,L246,D711,L641,D75,R756,U365,R203,D377,R624,U430,L422,U367,R547,U294,L916,D757,R509,D332,R106,D401,L181,U5,L443,U197,R406,D829,R878,U35,L958,U31,L28,D362,R188,D582,R358,U750,R939,D491,R929,D513,L541,U418,R861,D639,L917,U582,R211,U725,R711,D718,L673,U921,L157,U83,L199,U501,L66,D993,L599,D947,L26,U237,L981,U833,L121,U25,R641,D372,L757,D645,R287,U390,R274,U964,R288,D209,R109,D364,R983,U715,L315,U758,R36,D500,R626,U893,L840,U716,L606,U831,L969,D643,L300,D838,R31,D751,L632,D702,R468,D7,L169,U149,R893,D33,R816,D558,R152,U489,L237,U415,R434,D472,L198,D874,L351,U148,R761,U809,R21,D25,R586,D338,L568,U20,L157,U221,L26,U424,R261,D227,L551,D754,L90,U110,L791,U433,R840,U323,R240,U124,L723,D418,R938,D173,L160,U293,R773,U204,R192,U958,L472,D703,R556,D168,L263,U574,L845,D932,R165,D348,R811,D834,R960,U877,R935,D141,R696,U748,L316,U236,L796,D566,R524,U449,R378,U480,L79,U227,R867,D185,R474,D757,R366,U153,R882,U252,R861,U900,R28,U381,L845,U642,L849,U352,R134,D294,R788,D406,L693,D697,L433,D872,R78,D364,R240,U995,R48,D681,R727,D825,L583,U44,R743,D929,L616,D262,R997,D15,R575,U341,R595,U889,R254,U76,R962,D944,R724,D261,R608,U753,L389,D324,L569,U308,L488,D358,L695,D863,L712,D978,R149,D177,R92
L1003,D960,L10,D57,R294,U538,R867,D426,L524,D441,R775,U308,R577,D785,R495,U847,R643,D895,R448,U685,L253,U312,L312,U753,L89,U276,R799,D923,L33,U595,R400,U111,L664,D542,R171,U709,L809,D713,L483,U918,L14,U854,L150,D69,L158,D500,L91,D800,R431,D851,L798,U515,L107,U413,L94,U390,L17,U221,L999,D546,L191,U472,L568,U114,L913,D743,L713,D215,L569,D674,L869,U549,L789,U259,L330,D76,R243,D592,L646,U880,L363,U542,L464,D955,L107,U473,R818,D786,R852,U968,R526,D78,L275,U891,R480,U991,L981,D391,R83,U691,R689,D230,L217,D458,R10,U736,L317,D145,R902,D428,R344,U334,R131,D739,R438,D376,L652,U304,L332,D452,R241,D783,R82,D317,R796,U323,R287,D487,L302,D110,R233,U631,R584,U973,L878,D834,L930,U472,R120,U78,R806,D21,L521,U988,R251,D817,R44,D789,R204,D669,R616,D96,R624,D891,L532,U154,R438,U469,R785,D431,R945,U649,R670,D11,R840,D521,L235,D69,L551,D266,L454,U807,L885,U590,L647,U763,R449,U194,R68,U809,L884,U962,L476,D648,L139,U96,L300,U351,L456,D202,R168,D698,R161,U834,L273,U47,L8,D157,L893,D200,L454,U723,R886,U92,R474,U262,L190,U110,L407,D723,R786,D786,L572,D915,L904,U744,L820,D663,R205,U878,R186,U247,L616,D386,R582,U688,L349,D399,R702,U132,L276,U866,R851,D633,R468,D263,R678,D96,L50,U946,R349,D482,R487,U525,R464,U977,L499,D187,R546,U708,L627,D470,R673,D886,L375,U616,L503,U38,L775,D8,L982,D556,R159,U680,L124,U777,L640,D607,R248,D671,L65,D290,R445,U778,L650,U679,L846,D1,L769,U659,R734,D962,R588,U178,R888,D753,R223,U318,L695,D586,R430,D61,R105,U801,R953,U721,L856,U769,R937,D335,R895").











