-module(waiting_tree_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

case_1_test() -> % for "2+(4-1)"
    {WT, Term} = cases:waiting_tree("2+(4-1)"),
    E = {[#expression{ full= true,
		       op= ?tt2,
		       left= ?tt1,
		       right= #paren{ exp= #expression{ full= true,
							op= ?tt5,
							left= ?tt4,
							right= ?tt6 } } }],
	 ?tt7},
    ?assertEqual(E, {WT, Term}).

case_2_test() -> % for "2+(4-1)*3"
    {WT, Term} = cases:waiting_tree("2+(4-1)*3"),
    E = {[#expression{ full= true,
		       op= ?tt2,
		       left= ?tt1,
		       right= #expression{
				 full= true,
				 op= ?tt8,
				 left= #paren{
					  exp= #expression{
						  full= true,
						  op= ?tt5,
						  left= ?tt4,
						  right= ?tt6 }},
				 right= ?tt9 } }],
	 ?tt9},
    ?assertEqual(E, {WT, Term}).

