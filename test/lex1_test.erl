-module(lex1_test).
-include_lib("eunit/include/eunit.hrl").

lex1_test() ->
    Input1 = "(1 + 2) *5 / 7",
    Input2 = "(1 + 2) *5-32",
    Drop = formula_lex1:drop_common_leading(Input1, Input2),
    ?assertEqual({"(1 + 2) *5", " / 7", "-32"}, Drop),
    String = formula_lex1:appended_input("(1 + 2) *5 / 7", "(1 + 2) *5-32"),
    ?assertEqual(String, "<<<<-32").
