-module(acie_parser_reduce_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

-define(nt, #term{ type= ?numeral, loc= 3, value= "999" }).

case_1_test() ->
    % "2+(4-999"
    Case = [#expression{ full= true, op= ?tt5, left= ?tt4, right= ?nt },
	    #paren{},
	    #expression{ op= ?tt2, left= ?tt1 }],
    V = acie_parser:reduce(Case),
    E = [#expression{ full= true, op= ?tt5, left= ?tt4, right= ?nt },
	 #paren{},
	 #expression{ op= ?tt2, left= ?tt1 }],
    ?assertEqual(E, V).
