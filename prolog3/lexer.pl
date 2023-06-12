% vim: ft=prolog :

:- module(lexer, [lex/2]).

digit(Code) :- Code >= 48, Code =< 57, !.
letter(Code) :- Code >= 97, Code =< 122, !.
letter(Code) :- Code >= 65, Code =< 90, !.

start_valid_id(Code) :- letter(Code); Code = 95.
valid_id(Code) :- letter(Code); digit(Code); Code = 95.

lex(S, T) :- atom_codes(S, Codes), lex_impl(Codes, T).

lex_impl([], [eof]).
lex_impl([C,C1|Cs], Tokens) :-
    eq(C1),
    (
        lt(C), lex_impl(Cs, NewTokens), Tokens = [le|NewTokens];
        gt(C), lex_impl(Cs, NewTokens), Tokens = [ge|NewTokens];
        eq(C), lex_impl(Cs, NewTokens), Tokens = [eq|NewTokens]
    ), !.

lex_impl([C|Cs], Tokens) :-
    % eat whitespace
    whitespace(C), lex_impl(Cs, Tokens), !;

    % parse simple tokens
    plus(C),     lex_impl(Cs, NewTokens), Tokens = [plus    |NewTokens], !;
    minus(C),    lex_impl(Cs, NewTokens), Tokens = [minus   |NewTokens], !;
    star(C),     lex_impl(Cs, NewTokens), Tokens = [star    |NewTokens], !;
    lparen(C),   lex_impl(Cs, NewTokens), Tokens = [lparen  |NewTokens], !;
    rparen(C),   lex_impl(Cs, NewTokens), Tokens = [rparen  |NewTokens], !;
    bang(C),     lex_impl(Cs, NewTokens), Tokens = [bang    |NewTokens], !;
    lbracket(C), lex_impl(Cs, NewTokens), Tokens = [lbracket|NewTokens], !;
    rbracket(C), lex_impl(Cs, NewTokens), Tokens = [rbracket|NewTokens], !;
    semi(C),     lex_impl(Cs, NewTokens), Tokens = [semi    |NewTokens], !;
    comma(C),    lex_impl(Cs, NewTokens), Tokens = [comma   |NewTokens], !;
    lt(C),       lex_impl(Cs, NewTokens), Tokens = [lt      |NewTokens], !;
    gt(C),       lex_impl(Cs, NewTokens), Tokens = [gt      |NewTokens], !;
    eq(C),       lex_impl(Cs, NewTokens), Tokens = [assign  |NewTokens], !;

    % parse digit
    digit(C),
    parse_number([C | Cs], Rest, N, _),
    lex_impl(Rest, NewTokens),
    Tokens = [n(N) | NewTokens], !;

    % parse identifier
    start_valid_id(C),
    parse_identifier(Cs, Rest, RestId),
    lex_impl(Rest, NewTokens),
    IdCodes = [C | RestId],
    string_codes(Id, IdCodes),
    (function(Id) -> Tokens = [fn     | NewTokens], !;
     let(Id)      -> Tokens = [let    | NewTokens], !;
     true         -> Tokens = [id(Id) | NewTokens]
    ), !;

    % invalid
    string_codes(Id, [C]),
    Tokens = [inv(Id)], !.


parse_number([], _, 0, 0).
parse_number([C | Cs], Rest, N, R0) :- 
    digit(C),
    parse_number(Cs, Rest, M, R1),
    R0 is R1 * 10,
    N is M + (C - 48) * R1;

    Rest = [C | Cs], N = 0, R0 = 1.

parse_identifier([], _, []).
parse_identifier([C|Cs], Rest, RestId) :-
    valid_id(C), parse_identifier(Cs, Rest, RestId1), RestId = [C | RestId1], !;
    Rest = [C|Cs], RestId = [].































































































































































































































%        0 nul    1 soh    2 stx    3 etx    4 eot    5 enq    6 ack    7 bel
%        8 bs     9 ht    10 nl    11 vt    12 np    13 cr    14 so    15 si
%       16 dle   17 dc1   18 dc2   19 dc3   20 dc4   21 nak   22 syn   23 etb
%       24 can   25 em    26 sub   27 esc   28 fs    29 gs    30 rs    31 us
%       32 sp    33  !    34  "    35  #    36  $    37  %    38  &    39  '
%       40  (    41  )    42  *    43  +    44  ,    45  -    46  .    47  /
%       48  0    49  1    50  2    51  3    52  4    53  5    54  6    55  7
%       56  8    57  9    58  :    59  ;    60  <    61  =    62  >    63  ?
%       64  @    65  A    66  B    67  C    68  D    69  E    70  F    71  G
%       72  H    73  I    74  J    75  K    76  L    77  M    78  N    79  O
%       80  P    81  Q    82  R    83  S    84  T    85  U    86  V    87  W
%       88  X    89  Y    90  Z    91  [    92  \    93  ]    94  ^    95  _
%       96  `    97  a    98  b    99  c   100  d   101  e   102  f   103  g
%      104  h   105  i   106  j   107  k   108  l   109  m   110  n   111  o
%      112  p   113  q   114  r   115  s   116  t   117  u   118  v   119  w
%      120  x   121  y   122  z   123  {   124  |   125  }   126  ~   127 del

whitespace(9).
whitespace(10).
whitespace(14).
whitespace(32).

lparen(40).
rparen(41).
bang(33).
lbracket(123).
rbracket(125).
lt(60).
gt(62).
eq(61).
comma(44).
semi(59).

plus(43).
minus(45).
star(42).

function("fn").
let("let").

