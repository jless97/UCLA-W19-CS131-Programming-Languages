%  Tower Helper Functions

%  Transpose matrix
%  Source: https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

%  Set constraints (row/col size, domain, element uniqueness)
set_constraints(_, []).
set_constraints(N, [H|T]) :-
  length(H, N),
  fd_domain(H, 1, N),
  fd_all_different(H),
  set_constraints(N, T).

%  Check count by row
check_row([], Count, X, _) :-
  X = Count.
check_row([H|T], Count, X, Prev) :-
  (Prev < H ->
    Y is X + 1,
    check_row(T, Count, Y, H);
    check_row(T, Count, X, Prev)
  ).

%  Check count from a given view
check_tower([], []).
check_tower([Row|RT], [Count|CT]) :-
  check_row(Row, Count, 0, 0),
  check_tower(RT, CT).

%  Check count from all views
check_counts(T, T_tr, Top, Bottom, Left, Right) :-
  %  Left count
  check_tower(T, Left),

  %  Right count
  maplist(reverse, T, T_rev),
  check_tower(T_rev, Right),

  %  Top count
  check_tower(T_tr, Top),

  %  Bottom count
  maplist(reverse, T_tr, T_tr_rev),
  check_tower(T_tr_rev, Bottom).

%  Tower Implementation
tower(N, T, C) :-
  %  Set T constraints
  length(T, N),
  set_constraints(N, T),
  transpose(T, T_tr),
  set_constraints(N, T_tr),

  %  Set C constraints
  C = counts(Top, Bottom, Left, Right),
  length(Top, N),
  length(Bottom, N),
  length(Left, N),
  length(Right, N),

  %  Apply constraints
  maplist(fd_labeling, T),

  %  Check counts
  check_counts(T, T_tr, Top, Bottom, Left, Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Plain Tower Helper Functions 
%  Source: TA discussion slides
elements_between(Row, Min, Max) :-
  maplist(between(Min, Max), Row).

all_unique([]).
all_unique([H|T]) :- member(H, T), !, fail.
all_unique([_|T]) :- all_unique(T).

unique_row(N, Row) :-
  length(Row, N),
  elements_between(Row, 1, N),
  all_unique(Row).

set_constraints_plain(T, N) :-
  maplist(unique_row(N), T).

%  Plain Tower Implementation
plain_tower(N, T, C) :-
  %  Set T constraints
  length(T, N),
  set_constraints_plain(T, N),
  transpose(T, T_tr),
  set_constraints_plain(T_tr, N),

  %  Set C constraints
  C = counts(Top, Bottom, Left, Right),
  length(Top, N),
  length(Bottom, N),
  length(Left, N),
  length(Right, N),

  %  Check counts
  check_counts(T, T_tr, Top, Bottom, Left, Right).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Speedup
stats_tower(T) :-
  statistics(cpu_time, [Start|_]),
  tower(4, _, counts([4,3,2,1],[1,2,2,2],[4,3,2,1],[1,2,2,2])),
  statistics(cpu_time, [End|_]),
  T is End - Start + 1.

stats_plain_tower(T) :-
  statistics(cpu_time, [Start|_]),
  plain_tower(4, _, counts([4,3,2,1],[1,2,2,2],[4,3,2,1],[1,2,2,2])),
  statistics(cpu_time, [End|_]),
  T is End - Start.

speedup(Ratio) :-
  stats_tower(T),
  stats_plain_tower(PT),
  Ratio is PT / T.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Ambiguous Towers Implementation
ambiguous(N, C, T1, T2) :-
  tower(N, T1, C),
  tower(N, T2, C),
  T1 \= T2.
