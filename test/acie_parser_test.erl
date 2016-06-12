-module(acie_parser_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

add_term_3_init_case_1_test() ->

    Term = #term{ type= ?numeral, loc= 1, value= "1" },
    WT = acie_parser:add_term([], Term, undefined),
    EWT = [#expression{ left= Term }],
    ?assertEqual(EWT, WT).

add_term_3_init_case_2_test() ->

    Term1 = #term{ type= ?float, loc= 2, value= "3.14" },
    WT1 = acie_parser:add_term([], Term1, undefined),
    EWT1 = [#expression{ left= Term1 }],
    ?assertEqual(EWT1, WT1).
    

add_term_3_init_case_3_test() ->

    Term2 = #term{ type= ?lparan, loc= 3, value= "(" },
    WT2 = acie_parser:add_term([], Term2, undefined),
    EWT2 = [#expression{}, #expression{}],
    ?assertEqual(EWT2, WT2).


add_term_3_init_case_4_test() ->
    
    Term = #term{ type= ?rparan, loc= 4, value= ")" },
    WT = acie_parser:add_term([], Term, undefined),
    EWT = {error, bad_term},
    ?assertEqual(EWT, WT).

add_term_3_init_case_5_test() ->
    
    Term = #term{ type= ?op_add, loc= 5, value= "-" },
    WT = acie_parser:add_term([], Term, undefined),
    EWT = {error, bad_term},
    ?assertEqual(EWT, WT).

add_term_3_init_case_6_test() ->

    Term = #term{ type= ?op_mul, loc= 6, value= "*" },
    WT = acie_parser:add_term([], Term, undefined),
    EWT = {error, bad_term},
    ?assertEqual(EWT, WT).

add_term_3_case_1_test() ->

    Term = #term{ type= ?numeral, loc = 7, value= "12" },
    WT = [#expression{ left= Term }],
    WT1 = acie_parser:add_term(WT, Term, Term),
    EWT = {error, bad_term},
    ?assertEqual(EWT, WT1).

add_term_3_case_2_test() ->

    Term = #term{ type= ?numeral, loc = 8, value= "34" },
    Term1 = #term{ type= ?op_mul, loc= 1, value= "*" },
    WT = [#expression{ op= Term1, left= Term }],
    WT1 = acie_parser:add_term(WT, Term, Term1),
    EWT = [#expression{ full= true, op= Term1, left= Term, right= Term }],
    ?assertEqual(EWT, WT1).

add_term_3_case_3_test() ->

    Term = #term{ type= ?rparan, loc= 9, value= ")" },
    Term1 = #term{ type= ?numeral, loc= 3, value= "56" },
    WT = [Exp =
	      #expression{ full= true,
			   op= #term{ type= ?op_add, loc= 2, value= "+" },
			   left= #term{ type= ?float, loc= 1, value= "3,14" },
			   right= Term1 }],
    WT1 = acie_parser:add_term(WT, Term, Term1),
    EWT = [#expression{ left= Exp }],
    ?assertEqual(EWT, WT1).
