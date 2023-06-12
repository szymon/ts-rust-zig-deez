% vim: set ft=prolog:

:- use_module(lexer, [lex/2]).

check(Input, Expected) :-
    lex(Input, Tokens),
    (Tokens == Expected ->
        format("OK: ~s~n", [Input]);
        format("FAIL: ~s~n", [Input]),
        format("  Expected: ~q~n", [Expected]),
        format("  Got:      ~q~n", [Tokens])
    ).

test_me() :-
    check("+=<=>===!()", [plus, assign, le, ge, eq, bang, lparen, rparen, eof]),
    check(",;*+-", [comma, semi, star, plus, minus, eof]),
    check("prolog", [id("prolog"), eof]),
    check("let prolog = 1;", [let, id("prolog"), assign, n(1), semi, eof]),
    check("let add2 = fn(x) { x + 2; };", [
        let, id("add2"), assign, fn, lparen, id("x"), rparen, lbracket,
        id("x"), plus, n(2), semi, rbracket, semi, eof
    ]).

