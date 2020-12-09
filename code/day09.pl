
% OK, let's dust off the old DCGs and try using them
% instead of regexps

:- use_module(library(lists)).
:- use_module(library(dcg/basics)).

:- discontiguous test/2.

test(1, X):-
    solve_day09a('code/data/day09_test', 5, X).

% ?- test(1, X).
% X = 127 .

test(2, X):-
    solve_day09a('code/data/day09_input', 25, X).

% ?- test(2, X).
% X = 29221323 .

solve_day09a(FileName, PreambleLength, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(data(NumberList), Codes, []),
    length(Preamble, PreambleLength),
    append(Preamble, MainCode, NumberList),
    get_invalid_code(Preamble, MainCode, Out).

% Use a DCG to parse the input.
%
% Simple integer identifier

data([]) --> blanks.
data([I|Is]) --> integer(I), blanks, data(Is).

% in the recursive case, append the next code to the
% end of the list of previous codes, and remove the head
get_invalid_code(PreviousCodes, [Next|_Rest], Next):-
    \+ valid_code(Next, PreviousCodes).

get_invalid_code([Code|PreviousCodes], [Next|Rest], Out):-
    valid_code(Next, [Code|PreviousCodes]),
    append(PreviousCodes, [Next], Codes1),
    get_invalid_code(Codes1, Rest, Out).

% True if the code is valid, given the prior list members.
valid_code(Code, PreviousCodes):-
    append(_Codes1, [N1|Codes2], PreviousCodes),
    member(N2, Codes2),
    Code is N1 + N2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 02
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(3, X):-
    solve_day09b('code/data/day09_test', 5, X).

% ?- test(3, X).
% X = 62 .

test(4, X):-
    solve_day09b('code/data/day09_input', 25, X).

% ?- test(4, X).
% X = 4389369 .

% As part one, but with a coda to find the necessary
% contiguous range, and return the final sum.

solve_day09b(FileName, PreambleLength, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(data(NumberList), Codes, []),
    length(Preamble, PreambleLength),
    append(Preamble, MainCode, NumberList),
    get_invalid_code(Preamble, MainCode, InvalidCode),  % Part 1 to here

    append(_L1, CR, NumberList),
    append(ContiguousRange, _L2, CR),
    sum_list(ContiguousRange, InvalidCode),
    min_member(MinCode, ContiguousRange),
    max_member(MaxCode, ContiguousRange),
    Out is MinCode + MaxCode.

