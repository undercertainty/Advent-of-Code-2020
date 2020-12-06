% I'll put some useful predicates in this file.

:- use_module(library(pcre)).
:- use_module(library(apply)).
:- use_module(library(lists)).


every_nth_member(N, List, Out):-
    every_nth_member(N, N, List, X-X, Out).

every_nth_member(1, N, [Next|Rest], SoFar-[Next|Z], Out):-
    every_nth_member(N, N, Rest, SoFar-Z, Out).

every_nth_member(N1, N, [_|Rest], SoFar, Out):-
    N1>1,
    N2 is N1-1,
    every_nth_member(N2, N, Rest, SoFar, Out).

every_nth_member(_, _, [], Out-[], Out).

re_findall(Pattern, String, [], []):-
    re_split(Pattern, String, [_S1], []).

re_findall(Pattern, String, [S2|Matches], Dicts):-
    re_split(Pattern, String, [_S1, S2|SplitList], []),
    every_nth_member(2, SplitList, Matches),
    length([S2|Matches], N),
    list_of_n_items(N, Pattern, PatternList),
    list_of_n_items(N, [], OptionList),
    maplist(re_matchsub, PatternList, [S2|Matches], Dicts, OptionList).

list_of_n_items(0, _, []).
list_of_n_items(N, Item, [Item|Out]):-
    N>0,
    N1 is N-1,
    list_of_n_items(N1, Item, Out).

%% Turns out bloody prolog doesn't have builtin
%% support for converting binary to decimal...
%
% Fails if input isn't a nameable sequence of
% 1 and 0.
% Note that: name('01', [48, 49]).

binary_to_decimal(BinaryAtom, Int):-
    ground(BinaryAtom),
    name(BinaryAtom, BinaryListRev),
    reverse(BinaryListRev, BinaryList),
    binary_to_decimal(BinaryList, 1, Int).

binary_to_decimal([], _, 0).
binary_to_decimal([48|Bits], Pow, IntOut):-
    Pow1 is Pow*2,
    binary_to_decimal(Bits, Pow1, IntOut).
binary_to_decimal([49|Bits], Pow, IntOut):-
    Pow1 is Pow*2,
    binary_to_decimal(Bits, Pow1, IntOut1),
    IntOut is IntOut1+Pow.


% Get the contents of a file as a list of
% strings
read_file_to_list(FileName, Out):-
    read_file_to_string(FileName, InputString, []),
    split_string(InputString, "\n", "", Out).

