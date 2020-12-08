
% OK, let's dust off the old DCGs and try using them
% instead of regexps

:- use_module(library(lists)).
:- use_module(library(dcg/basics)).

:- discontiguous test/2.

test(1, X):-
    solve_day08a('code/data/day08_test', X, part(a)).

% ?- test(1, X).
% X = 5 .

test(2, X):-
    solve_day08a('code/data/day08_input', X, part(a)).

% ?- test(2, X).
% X = 1420 .

solve_day08a(FileName, Out, part(a)):-
    read_file_to_codes(FileName, Codes, []),
    phrase(game_code(GameCode), Codes, []),
    execute_code(GameCode, 0, 0, [], Out, [part(a)]).

% Use a DCG to parse the input.
%
% ?- phrase(game_code(GC), `nop +0\nacc +1\njmp -3\n`, []).
% GC = [nop:0, acc:1, jmp: -3]

game_code([]) --> [].
game_code([I|Is]) --> instruction(I), 
                      "\n",
                      game_code(Is).

instruction(Op:Arg) --> operation(Op),
                          blank,
                          integer(Arg).

operation(nop) --> "nop".
operation(acc) --> "acc".
operation(jmp) --> "jmp".

% For part A, return the value of the 
% accumulator when there's a loop
execute_code(_, Instr, Acc, Visited, Acc, Options):-
    member(Instr, Visited),
    member(part(a), Options),
    !.

% For part B, fail when there's a loop, but
% don't need to be explicit 'cos it's in the
% recursive case options. However, if the
% index is out of range, then return the 
% value of the accumulator.
execute_code(Code, Instr, Acc, _Visited, Acc, Options):-
    length(Code, L),
    Instr >= L,
    member(part(b), Options).

execute_code(Code, Instr, Acc, Visited, Out, Options):-
    \+ member(Instr, Visited),
    nth0(Instr, Code, nop:_Arg),
    NextInstr is Instr+1,
    execute_code(Code, NextInstr, Acc, [Instr|Visited], Out, Options).

execute_code(Code, Instr, Acc, Visited, Out, Options):-
    \+ member(Instr, Visited),
    nth0(Instr, Code, jmp:Arg),
    NextInstr is Instr+Arg,
    execute_code(Code, NextInstr, Acc, [Instr|Visited], Out, Options).

execute_code(Code, Instr, Acc, Visited, Out, Options):-
    \+ member(Instr, Visited),
    nth0(Instr, Code, acc:Arg),
    NextInstr is Instr+1,
    Acc1 is Acc + Arg,
    execute_code(Code, NextInstr, Acc1, [Instr|Visited], Out, Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 02
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(3, X):-
    solve_day08b('code/data/day08_test', X, part(b)).

% ?- test(3, X).
% X = 8 .

test(4, X):-
    solve_day08b('code/data/day08_input', X, part(b)).

% ?- test(4, X).
% X = 1245 .

% The only change to the main execution part of the 
% task is the termination condition, so added an option
% for different behaviours at the end.

solve_day08b(FileName, Out, part(b)):-
    read_file_to_codes(FileName, Codes, []),
    phrase(game_code(GameCode), Codes, []),
    % Replace a nop with a jmp:
    select(nop:ARG, GameCode, jmp:ARG, ModifiedGameCode),
    execute_code(ModifiedGameCode, 0, 0, [], Out, [part(b)]).

solve_day08b(FileName, Out, part(b)):-
    read_file_to_codes(FileName, Codes, []),
    phrase(game_code(GameCode), Codes, []),
    % Replace a jmp with a nop:
    select(jmp:ARG, GameCode, nop:ARG, ModifiedGameCode),
    execute_code(ModifiedGameCode, 0, 0, [], Out, [part(b)]).
