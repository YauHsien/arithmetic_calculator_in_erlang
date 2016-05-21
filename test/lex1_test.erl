-module(lex1_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/lex.hrl").

lex1_append_input_test() ->
    Input1 = "(1 + 2) *5 / 7",
    Input2 = "(1 + 2) *5-32",
    LocAndString = formula_lex1:appended_input(Input1, Input2),
    ?assertEqual({11, "<<<<-32"}, LocAndString).

lex1_append_diff_backspace_test() ->
    R1 = formula_lex1:append_diff("", 1, "<"),
    ?assertEqual([], R1),
    R2 = formula_lex1:append_diff(
	   [formula_lex1:build_term(op_add, 1, "+"),
	    formula_lex1:build_term(numeral, 2, "30")],
	   3,
	   "<"),
    ?assertEqual([formula_lex1:build_term(op_add, 1, "+"),
		  formula_lex1:build_term(numeral, 2, "3")],
		 R2),
    R3 = formula_lex1:append_diff(R2, 3, "<"),
    ?assertEqual([formula_lex1:build_term(op_add, 1, "+")],
		 R3).
