% First part looks suspiciously straightforward

:- use_module(library(lists)).
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).

:- discontiguous test/2.

test(1, X):-
    solve_day10a('code/data/day10_testa', X).

% ?- test(1, X).
% X = 35 .

test(2, X):-
    solve_day10a('code/data/day10_input', X).

% ?- test(2, X).
% X = 1914 .

solve_day10a(FileName, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(data(NumberList), Codes, []),
    max_member(MaxMember, NumberList),
    Device is MaxMember + 3,
    sort([0, Device|NumberList], SortedList),
    adjacent_differences(SortedList, DifferencesList),
    count_list_members(DifferencesList, CountDict),
    Out is CountDict.1 * CountDict.3.

% Use a DCG to parse the input.
%
% Simple integer identifier

data([]) --> blanks.
data([I|Is]) --> integer(I), blanks, data(Is).

% adjacent_differences([1, 3, 5, 9], [2, 2, 4]).

adjacent_differences([X, Y], [Out]):-
    Out is Y-X.
adjacent_differences([X, Y|R], [Out|ROut]):-
    Out is Y-X,
    adjacent_differences([Y|R], ROut).

% count_list_members([a, b, c, 3, 2, c, b, a, a], X{2:1, 3:1, a:3, b:2, c:2}).

count_list_members([], _{}).
count_list_members([X|Rest], DictOut):-
    count_list_members(Rest, CountDict),
    get_dict(X, CountDict, Count, DictOut, Count1),
    Count1 is Count+1.
count_list_members([X|Rest], DictOut):-
    count_list_members(Rest, CountDict),
    \+ get_dict(X, CountDict, _),
    put_dict(X, CountDict, 1, DictOut).
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 02
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(3, X):-
    solve_day10b('code/data/day10_testa', X).

% ?- test(3, X).
% X = 62 .

test(4, X):-
    solve_day10b('code/data/day10_testb', X).

% ?- test(3, X).
% X = 62 .

test(5, X):-
    solve_day10b('code/data/day10_input', X).

% ?- test(4, X).
% X = 4389369 .

% OK... I imagine that this isn't going to work by
% brute force. Playing on paper a bit, perhaps
% we can make use of the fixed points? So which
% members of the list can't be safely removed?
% That will then partition the list into workable
% blocks.

solve_day10b(FileName, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(data(NumberList), Codes, []),
    max_member(MaxMember, NumberList),
    Device is MaxMember + 3,
    sort([0, Device|NumberList], SortedList),
    find_fixed_subsequences(SortedList, FixedSubSequences),
    maplist(find_all_valid_sequences, FixedSubSequences, ValidSubSequences),
    maplist(length, ValidSubSequences, SubSequenceLengths),
    foldl(product, SubSequenceLengths, 1, Out).


% valid_sequence(+ListIn) true if all adjacent
% differences are within 3.
%
% Assume that ListIn is a sorted list of ints.

valid_sequence(ListIn):-
    adjacent_differences(ListIn, AdjList),
    max_list(AdjList, M),
    M < 4.

% find_valid_sequences(+ListIn, -SeqOut).
%
% ?- find_valid_sequences([1, 3, 4, 6], X).
% X = [1, 3, 4, 6] ;
% X = [1, 4, 6] ;
% X = [1, 3, 6] ;
% false.
%
% SeqOut is a valid sequence in ListIn. Succeeds on
% backtracking. Complete brute force here, and no
% checks for repetition.

find_valid_sequences(ListIn, ListIn):-
    valid_sequence(ListIn).
find_valid_sequences([Head|Body], ValidSubSequence):-
    append(L1, [_RemovedElement, H2|R2], Body),
    append([Head|L1], [H2|R2], SubSequence),
    valid_sequence(SubSequence),
    find_valid_sequences(SubSequence, ValidSubSequence).

find_all_valid_sequences(ListIn, Out):-
    setof(X, find_valid_sequences(ListIn, X), Out).

% [0,1,4,5,6,7,10,11,12,15,16,19,22]

% find_fixed_subsequences([1, 3, 4, 6, 8, 9, 11], [[1, 3, 4, 6], [6, 8, 9, 11]])
%
% Find partitions where the endpoints can't be removed

find_fixed_subsequences(ListIn, FixedSubSequences):-
    find_fixed_subsequences(ListIn, ListIn, FixedSubSequences).

find_fixed_subsequences([Last], _, [[Last]]).
find_fixed_subsequences([Next, Next1|Rest], Sequence, [[Next], [Next|R1]|RestOut]):-
    \+ (select(Next, Sequence, S1),
         valid_sequence(S1)),
    find_fixed_subsequences([Next1|Rest], Sequence, [R1|RestOut]).
find_fixed_subsequences([Next, Next1|Rest], Sequence, [[Next|R1]|RestOut]):-
    select(Next, Sequence, S1),
    valid_sequence(S1),
    find_fixed_subsequences([Next1|Rest], Sequence, [R1|RestOut]).

product(X, Y, Z):-Z is (X * Y).