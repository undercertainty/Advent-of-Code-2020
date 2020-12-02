% Still pathologically using prolog

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pcre)).

% Get the input as a list of strings

get_input_as_list(FileName, Out):-
    open(FileName, read, Stream),
    read_string(Stream, _Length, FileContents),
    split_string(FileContents, "\n", "", Out),
    close(Stream).

% If we split the password by the given character,
% the resulting string has n+1 members, where n is
% the number of occurences of the character in the 
% string.

valid_password(StrIn):-
    re_matchsub("(?<min>\\d+)\\-(?<max>\\d+)\\s+(?<char>\\w):\\s+(?<str>\\w+)", StrIn, Match, []),
    number_string(Min, Match.min),
    number_string(Max, Match.max),
    split_string(Match.str, Match.char, "", L),
    length(L, Len),
    N is Len-1,
    N >= Min,
    N =< Max.

solve_day02a(FileName, Out):-
    get_input_as_list(FileName, Input),
    partition(valid_password, Input, Valid, _Invalid),
    length(Valid, Out).

test1(Out):-
    solve_day02a('code/data/day02_test', Out).

/*******************************
 
?- solve_day02a('code/data/day02_input', Out).
Out = 538.

 ********************************/

% OK, part 2...

% Slightly different problem, but can use
% the same regular expression.

% Convert the input string to a list of 
% character codes and check on that.
valid_password_b(StrIn):-
    re_matchsub("(?<min>\\d+)\\-(?<max>\\d+)\\s+(?<char>\\w):\\s+(?<str>\\w+)", StrIn, Match, []),
    number_string(Min, Match.min),
    number_string(Max, Match.max),
    name(Match.char, [Char]),
    name(Match.str, Chars),
    valid_password_b_(Char, Chars, Min, Max).

valid_password_b_(Char, Chars, Pos1, Pos2):-
    nth1(Pos1, Chars, Char),
    \+ nth1(Pos2, Chars, Char).

valid_password_b_(Char, Chars, Pos1, Pos2):-
    \+ nth1(Pos1, Chars, Char),
    nth1(Pos2, Chars, Char).


solve_day02b(FileName, Out):-
    get_input_as_list(FileName, Input),
    partition(valid_password_b, Input, Valid, _Invalid),
    length(Valid, Out).

test2(Out):-
    solve_day02b('code/data/day02_test', Out).

/*******************************
 
?- solve_day02b('code/data/day02_input', Out).
Out = 489.

 ********************************/