
% OK, let's dust off the old DCGs and try using them
% instead of regexps

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

:- ['code/aoc2020.pl'].

test01(X):-
    solve_day07a('code/data/day07_test', shiny-gold, X).

% ?- test01(X).
% X = 4 .

test02(X):-
    solve_day07a('code/data/day07_input', shiny-gold, X).

% ?- test02(X).
% X = 6531 .

solve_day07a(FileName, Bag, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(bag_rules(BagRules), Codes, []),
    findall(C, contains_bag(Bag, BagRules, [C|_DL]/[]), Bs),
    list_to_set(Bs, Ss),
    length(Ss, Out).

% Use a DCG to parse the input.
%
% ?- phrase(bag_rules(BagRules), `light red bags contain 1 bright white bag, 2
% muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted 
% yellow bags.\n`, []).
%
% BagRules = [light-red:[(1, bright-white),  (2, muted-yellow)], 
%             dark-orange:[(3, bright-white),  (4, muted-yellow)]] .

bag_rules([]) --> [].
bag_rules([Rule|Rules]) --> bag_rule(Rule), "\n", bag_rules(Rules).
bag_rule(Adj-Colour:Contained) --> word(Adj), 
                                   blank, 
                                   word(Colour), 
                                   " bags contain ",
                                   (
                                       bag_rhs_rule(Contained);
                                      ("no other bags.", {Contained=[]})
                                    ).
bag_rhs_rule([]) --> [].
bag_rhs_rule([(Int, Adj-Colour)|Rules]) --> blanks,
                               integer(Int),
                               blank,
                               word(Adj),
                               blank,
                               word(Colour),
                               blank,
                                    ("bag" ; "bags"),
                                    ("," ; "."),
                                bag_rhs_rule(Rules).
word(Atom) --> nonblanks(Codes),
                {atom_codes(Atom, Codes)}.
        
contains_bag(Bag, Rules, [Container|X]/X):-
    select(Container:Contains, Rules, _),
    member((_N, Bag), Contains).

contains_bag(Bag, Rules, Out/O):-
    select(Container:Contains, Rules, R1),
    member((_N, Bag), Contains),
    contains_bag(Container, R1, Out/[Container|O]).

                        
%%%%%%%%%%%%%%%%%%%%%%%
%
% Part 02
%
%%%%%%%%%%%%%%%%%%%%%%%%

test03(X):-
    solve_day07b('code/data/day07_test', shiny-gold, X).

% ?- test03(X).
% X = 32 ;

test04(X):-
    solve_day07b('code/data/day07b_test', shiny-gold, X).

% ?- test04(X).
% X = 126 ;


test05(X):-
    solve_day07b('code/data/day07_input', shiny-gold, X).

% ?- test05(X).
% X = 18925 

% For this one, need to find which members
% are in all the sets:

% ?- test04(X).
% X = 3143 ;

solve_day07b(FileName, Bag, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(bag_rules(BagRules), Codes, []),
    member(Bag:Daughters, BagRules),
    filled_bag_size(Daughters, BagRules, ContentsSizeIncludingContainingBag),
    Out is ContentsSizeIncludingContainingBag-1.

filled_bag_size([], _Rules, 1).
filled_bag_size([(Num, BagDaughter)|Rest], Rules, Out):-
    member(BagDaughter:Ds, Rules),
    filled_bag_size(Ds, Rules, DsSize),
    SizeOfNumDaughters is DsSize * Num,
    filled_bag_size(Rest, Rules, SizeOfRestOfDaughters),
    Out is SizeOfNumDaughters+SizeOfRestOfDaughters.


