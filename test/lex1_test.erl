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
    R1 = formula_lex1:append_diff("", 1, "()12+/34()*-56/*78", SendTo),
    ?assertEqual([{term,lparan,1,undefined},
		  {term,rparan,2,undefined},
		  {term,numeral,3,"12"},
		  {term,op_mul,6,"/"},
		  {term,numeral,7,"34"},
		  {term,lparan,9,undefined},
		  {term,rparan,10,undefined},
		  {term,op_add,12,"-"},
		  {term,numeral,13,"56"},
		  {term,op_mul,16,"*"},
		  {term,numeral,17,"78"}],
		R1).

lex1_append_diff_formula_consecutive_test() ->
    SendTo = self(),
    R1 = formula_lex1:append_diff("", 1, "( a 1.2+34)*56/78", SendTo),
    Expected = [{term,?lparan,1,undefined},
		  {term,?not_accepted,3,"a"},
		  {term,?float,5,"1.2"},
		  {term,?op_add,8,"+"},
		  {term,?numeral,9,"34"},
		  {term,?rparan,11,undefined},
		  {term,?op_mul,12,"*"},
		  {term,?numeral,13,"56"},
		  {term,?op_mul,15,"/"},
		  {term,?numeral,16,"78"}],
    ?assertEqual(Expected, R1).

lex1_append_diff_formula_consecutive_spaces_test() ->
    SendTo = self(),
    R1 = formula_lex1:append_diff([], 1, "   ", SendTo),
    E = [],
    ?assertEqual(E, R1).

lex1_append_diff_formula_consecutive_points_test() ->
    SendTo = self(),
    R1 = formula_lex1:append_diff([], 1, "...", SendTo),
    E = [formula_lex1:build_term(?not_accepted, 1, "...")],
    ?assertEqual(E, R1).

lex1_append_diff_formula_consecutive_numbers_test() ->
    SendTo = self(),
    R1 = formula_lex1:append_diff([], 1, " 12 3.4 4.2.5 ", SendTo),
    E = [formula_lex1:build_term(?numeral, 2, "12"),
	 formula_lex1:build_term(?float, 5, "3.4"),
	 formula_lex1:build_term(?not_accepted, 9, "4.2.5")],
    ?assertEqual(E, R1).

lex1_append_diff_formula_consecutive_operators_test() ->
    SendTo = self(),
    R1 = formula_lex1:append_diff([], 1, " + /- ", SendTo),
    E = [formula_lex1:build_term(?op_add, 5, "-")],
    ?assertEqual(E, R1).
