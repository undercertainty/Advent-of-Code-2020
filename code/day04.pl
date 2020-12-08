% Still pathologically using prolog

:- use_module(library(apply)).
:- use_module(library(occurs)).
:- use_module(library(lists)).

% Get the list of  input structures

get_input(FileName, OutputString):-
    read_file_to_string('code/data/day04_test', Out, []), re_split('\n\n', Out, O).






% I think it will be useful to have a predicate which creates a 
% list of modulo values:

create_modulo_list(Length, Start, Step, Floor, Out):-
    Start1 is Start mod Floor,
    create_modulo_list(Length, Start1, Step, Floor, [], Out).

create_modulo_list(0, _, _, _, RevOut, Out):-
    reverse(RevOut, Out).
create_modulo_list(N, Pos, Step, Floor, SoFar, Out):-
    N>0,
    N1 is N-1,
    Pos1 is (Pos+Step) mod Floor,
    create_modulo_list(N1, Pos1, Step, Floor, [Pos|SoFar], Out).


% Get the input as a list of strings

get_input_as_list(FileName, Out):-
    open(FileName, read, Stream),
    read_string(Stream, _Length, FileContents),
    split_string(FileContents, "\n", "\r", Out), % Seem to need "\r" for windows
    close(Stream).


% To solve, want to know what the n0th character
% is for each row of the output

solve_day03a(FileName, Out):-
    get_input_as_list(FileName, Lines),
    maplist(string_chars, Lines, [First|Rest]),
    length(Lines, N),
    length(First, Floor),
    create_modulo_list(N, 0, 3, Floor, ModList),
    maplist(nth0, ModList, [First|Rest], ListOut),
    occurrences_of_term(#, ListOut, Out).


test_day03a(Out):-
    solve_day03a('code/data/day03_test', Out).

/*******************************
 
?- solve_day03a('code/data/day03_input', Out).
Out = 176

 ********************************/

% OK, part 2...

% Looks as though we need to step down the vertical
% axis, as well as move along the horizontal axis.

% So if we're moving Dy vertically at each step, want to
% step down Dy and across Dx for each step. This is 
% easier than I made it before...

% find_path(+StartX, +StartY, +Dx, +Dy, +Map, -ListOfPoints)

find_path(X, Y, Dx, Dy, FileName, Path):-
    get_input_as_list(FileName, FileAsListOfStrings),
    maplist(string_chars, FileAsListOfStrings, [MapHead|MapRest]),
    length([MapHead|MapRest], Height),
    length(MapHead, Width),
    find_path(X, Y, Dx, Dy, [MapHead|MapRest], Width, Height, DL-DL, Path).

find_path(X, Y, Dx, Dy, Map, Width, Height, SoFar-[P|DL], Out):-
    Y<Height,
    nth0(Y, Map, Row),
    nth0(X, Row, P),
    X1 is (X+Dx) mod Width,
    Y1 is Y+Dy,
    find_path(X1, Y1, Dx, Dy, Map, Width, Height, SoFar-DL, Out).

find_path(_, Y, _Dx, _Dy, _Map, _Width, Height, Out-[], Out):-
    Y>=Height.

% Quick function to multiply the members
% of a list together:
list_product([X], X).
list_product([Next|Rest], Out):-
    list_product(Rest, R1),
    Out is R1 * Next.

% Now, for the whole part b,  call with lists of coords:

solve_day03b(FileName, ListOfCoords, Out):-
    solve_day_03b_(FileName, ListOfCoords, ListOutputs),
    list_product(ListOutputs, Out).

solve_day_03b_(_FileName, [], []).
solve_day_03b_(FileName, [(Dx, Dy)|Rest], [TreesInPath|Out]):-
    find_path(0, 0, Dx, Dy, FileName, Path),
    occurrences_of_term(#, Path, TreesInPath),
    solve_day_03b_(FileName, Rest, Out).

test2(Out):-
    solve_day03b('code/data/day03_test', [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)], Out).

/*******************************
 
?- solve_day03b('code/data/day03_input', [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)], Out).
Out = 5872458240 .

 ********************************/