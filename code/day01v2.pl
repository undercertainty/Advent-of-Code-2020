% Use the apply library to map strings from the
% input into integers
%
% Doing a second version to use regexps
:- use_module(library(apply)).
:- use_module(library(pcre)).

% God, I'd forgotten just how fiddly IO is in
% prolog. This would be one line in python...

get_input(FileName, Out):-
    open(FileName, read, Stream),
    read_string(Stream, _Length, Out),
    close(Stream).

re_findall(Regex, Str, Out):-
    re_split(Regex, Str, SplitList),
    re_findall1(SplitList, Out).

re_findall(Regex, Str, Out, Options):-
    re_split(Regex, Str, SplitList, Options),
    re_findall1(SplitList, Out).

re_findall1([], []).
re_findall1([_], []).
re_findall1([_, Next|Rest], [Next|Out]):-
    re_findall1(Rest, Out).


% Find the two entries which sum to 2020
find_entries(ListIn, X, Y):-
    member(X, ListIn),
    member(Y, ListIn),
    2020 is X + Y,
    !. % Cut for minor efficiency gain

solve_day01a(FileName, Out):-
    get_input(FileName, Input),
    re_findall('\\d+', Input, SplitInput),
    maplist(number_string, ParsedInput, SplitInput),
    find_entries(ParsedInput, X, Y),
    Out is X*Y.

test1(Out):-
    solve_day01a('code/data/day01_test', Out).

/*******************************
 
?- solve_day01a('code/data/day01_input', Out).
Out = 158916.

 ********************************/

% OK, part 2...

% Same problem, but this time using three entries.
% I suppose I ought to generalise, but I don't 
% really have time, and I'm sure I'll spend more
% time than I want on this in the next month...

% Find the three entries which sum to 2020
find_entries(ListIn, X, Y, Z):-
    member(X, ListIn),
    member(Y, ListIn),
    member(Z, ListIn),
    2020 is X + Y + Z,
    !. % Cut for minor efficiency gain

solve_day01b(FileName, Out):-
    get_input(FileName, Input),
    re_findall('\\d+', Input, SplitInput),
    maplist(number_string, ParsedInput, SplitInput),
    find_entries(ParsedInput, X, Y, Z),
    Out is X*Y*Z.

test2(Out):-
    solve_day01b('code/data/day01_test', Out).

/*******************************
 
?- solve_day01b('code/data/day01_input', Out).
Out = 165795564.

 ********************************/
