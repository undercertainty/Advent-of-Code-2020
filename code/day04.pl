
:- use_module(library(lists)).
:- use_module(library(pcre)).
:- use_module(library(apply)).

:- ['code/aoc2020.pl'].

test01(X):-
    solve_day04a('code/data/day04_test', X).

test02(X):-
    solve_day04a('code/data/day04_input', X).

% ?- test02(X).
% X = 226 .

solve_day04a(FileName, Out):-
    get_input(FileName, InputString),
    re_split('\n\n', InputString, [SI|SplitInput]),
    every_nth_member(2, SplitInput, PassportStrings),
    maplist(string_to_passport_dict, [SI|PassportStrings], PassportDicts),
    partition(valid_passport, PassportDicts, ValidPassports, _),
    length(ValidPassports, Out).


% Start by loading the input string:

get_input(FileName, FileContents):-
    open(FileName, read, Stream),
    read_string(Stream, _Length, FileContents),
    close(Stream).

% Convert a string representation of a passport to a
% dictionary representation

string_to_passport_dict(String, Dict):-
    re_findall('(?P<field>\\w+):(?P<value>\\S+)', String, _Matches, Dicts),
    convert_passport_dict(Dicts, Dict).

% Something I don't get about dict keys that it's too
% late to figure out now, so I'll just go with a list
% of pairs

convert_passport_dict([Dict|Dicts], [Key:Dict.value|Out]):-
    string_to_atom(Dict.field, Key),
    convert_passport_dict(Dicts, Out).
convert_passport_dict([], []).

% A dictionary-encoded passport is valid if it contains
% each of the fields ecl, pid, eyr, hcl, byr, iyr
% cid, hgt
% although for the first part, we're cheating with cid

valid_passport(DictPassport):-
    subset([ecl:_, pid:_, eyr:_, hcl:_, byr:_, iyr:_, hgt:_], DictPassport).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 02
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Blimey, that was dull. Anyway...

test_03(X):-
    solve_day04b('code/data/day04_input', X).

% ?- test_03(X).
% X = 160 .

solve_day04b(FileName, Out):-
    get_input(FileName, InputString),
    re_split('\n\n', InputString, [SI|SplitInput]),
    every_nth_member(2, SplitInput, PassportStrings),
    maplist(string_to_passport_dict, [SI|PassportStrings], PassportDicts),
    partition(valid_passport_b, PassportDicts, ValidPassports, _),
    length(ValidPassports, Out).


% OK, this is just a slog. We'll go through the
% fields in the order they're given in the spec:

valid_passport_b(DictPassport):-
    valid_passport_byr(DictPassport),
    valid_passport_iyr(DictPassport),
    valid_passport_eyr(DictPassport),
    valid_passport_hgt(DictPassport),
    valid_passport_ecl(DictPassport),
    valid_passport_hcl(DictPassport),
    valid_passport_pid(DictPassport).

% Birth year first
valid_passport_byr(DictPassport):-
    member(byr:BYR_S, DictPassport),
    string_to_atom(BYR_S, BYR_A),
    atom_number(BYR_A, BYR),
    integer(BYR),
    BYR>=1920,
    BYR=<2002.

valid_passport_iyr(DictPassport):-
    member(iyr:IYR_S, DictPassport),
    string_to_atom(IYR_S, IYR_A),
    atom_number(IYR_A, IYR),
    integer(IYR),
    IYR>=2010,
    IYR=<2020.

valid_passport_eyr(DictPassport):-
    member(eyr:EYR_S, DictPassport),
    string_to_atom(EYR_S, EYR_A),
    atom_number(EYR_A, EYR),
    integer(EYR),
    EYR>=2020,
    EYR=<2030.

valid_passport_hgt(DictPassport):-
    member(hgt:HGT_S, DictPassport),
    name(HGT_S, HGT_LS),
    name("in", IN_LS),
    append(HGT_N_LS, IN_LS, HGT_LS),
    name(HGT_N, HGT_N_LS),
    number(HGT_N),
    HGT_N>=59,
    HGT_N=<76.

valid_passport_hgt(DictPassport):-
    member(hgt:HGT_S, DictPassport),
    name(HGT_S, HGT_LS),
    name("cm", IN_LS),
    append(HGT_N_LS, IN_LS, HGT_LS),
    name(HGT_N, HGT_N_LS),
    number(HGT_N),
    HGT_N>=150,
    HGT_N=<193.

valid_passport_hcl(DictPassport):-
    member(hcl:HCL_STR, DictPassport),
    re_match('^#[0-9a-f]{6}$', HCL_STR).

valid_passport_ecl(DictPassport):-
    member(ecl:ECL_STR, DictPassport),
    member(ECL_STR, ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]).

valid_passport_pid(DictPassport):-
    member(pid:PID_STR, DictPassport),
    re_match('^\\d{9}$', PID_STR).



