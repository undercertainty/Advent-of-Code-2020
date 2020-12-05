
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(apply)).

:- ['code/aoc2020.pl'].

test01(X):-
    solve_day04a('code/data/day05_test', X).

% ?- test01(X).
% X = 820.

test02(X):-
    solve_day04a('code/data/day05_input', X).

% ?- test02(X).
% X = 226 .

solve_day04a(FileName, Out):-
    get_input_as_list(FileName, InputStrings),
    maplist(string_to_seat_id, InputStrings, SeatIDs),
    max_list(SeatIDs, Out).

string_to_seat_id(SeatString, SeatID):-
    re_replace('F'/g, '0', SeatString, SS1),
    re_replace('B'/g, '1', SS1, SS2),
    re_replace('L'/g, '0', SS2, SS3),
    re_replace('R'/g, '1', SS3, SeatStringBin),
    name(SeatStringBin, SeatStringBinList),
    length(RowBinList, 7),
    append(RowBinList, ColBinList, SeatStringBinList),
    name(RowAtom, RowBinList),
    name(ColAtom, ColBinList),
    binary_to_decimal(RowAtom, RowDec),
    binary_to_decimal(ColAtom, ColDec),
    SeatID is 8 * RowDec + ColDec.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 2
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test03(X):-
    solve_day04b('code/data/day05_input', X).

% ?- test03(X).
% X = 527 ;
% false.

% OK, if my seat ± 1 is in the list, just sort the
% IDs of the seats in the list, and find the gap:

solve_day04b(FileName, Out):-
    get_input_as_list(FileName, InputStrings),
    maplist(string_to_seat_id, InputStrings, ListOfSeatIDs),
    sort(ListOfSeatIDs, SortedList),
    nth1(N, SortedList, SeatIdBefore),
    N1 is N+1, 
    SeatIdAfter is SeatIdBefore+2, 
    nth1(N1, SortedList, SeatIdAfter),
    Out is SeatIdBefore+1.
