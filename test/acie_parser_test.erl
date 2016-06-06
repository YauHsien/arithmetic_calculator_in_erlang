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
    EWT2 = [#expression{}],
    ?assertEqual(EWT2, WT2).


add_term_3_init_case_4_test() ->
    
    Term = #term{ type= ?lparan, loc= 4, value= ")" },
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
