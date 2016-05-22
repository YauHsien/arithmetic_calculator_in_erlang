-module(lex1_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/lex.hrl").

lex1_append_input_test() ->
    Input1 = "(1 + 2) *5 / 7",
    Input2 = "(1 + 2) *5-32",
    LocAndString = formula_lex1:appended_input(Input1, Input2),
    ?assertEqual({4, 11, "-32"}, LocAndString).


lex1_backspace_terms_test() ->
    SendTo = self(),

    R2 = formula_lex1:backspace_terms(
	   [formula_lex1:build_term(op_add, 1, "+"),
	    formula_lex1:build_term(numeral, 2, "30")], 1, SendTo),
    ?assertEqual([formula_lex1:build_term(op_add, 1, "+"),
		  formula_lex1:build_term(numeral, 2, "3")],
		 R2),

    R3 = formula_lex1:backspace_terms(R2, 1, SendTo),
    ?assertEqual([formula_lex1:build_term(op_add, 1, "+")],
		 R3),

    R4 = formula_lex1:backspace_terms(R2, 2, SendTo),
    ?assertEqual([], R4).


lex1_append_diff_formula_test() ->
    SendTo = self(),
    R1 = formula_lex1:append_diff("", 1, "(12+34)*56/78", SendTo),
    ?assertEqual([{term,lparan,1,undefined},
		  {term,numeral,2,"12"},
		  {term,op_add,4,"+"},
		  {term,numeral,5,"34"},
		  {term,rparan,7,undefined},
		  {term,op_mul,8,"*"},
		  {term,numeral,9,"56"},
		  {term,op_mul,11,"/"},
		  {term,numeral,12,"78"}],
		R1).
