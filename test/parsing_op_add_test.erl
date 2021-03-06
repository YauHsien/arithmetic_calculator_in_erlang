-module(parsing_op_add_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

-define(nt, #term{ type= ?op_add, loc= 3, value= "+" }).
%-define(nt, #term{ type= ?op_add, loc= 5, value= "-" }).

expected_values() ->
    [ {"",          {error, bad_term}},
      {"2",         [#expression{ op= ?nt,
				  left= ?tt1 }]},
      {"2+",        {error, bad_term}},
      {"2+(",       {error, bad_term}},
      {"2+(4",      [#expression{ op= ?nt,
				  left= ?tt4 },
		     #paren{},
		     #expression{ op= ?tt2,
				  left= ?tt1 }]},
      {"2+(4-",     {error, bad_term}},
      {"2+(4-1",    [#expression{ op= ?nt,
				  left= #expression{ full= true,
						     op= ?tt5,
						     left= ?tt4,
						     right= ?tt6 } },
		     #paren{},
		     #expression{ op= ?tt2,
				  left= ?tt1 }]},
      {"2+(4-1)",   [#expression{
			op= ?nt,
			left= #expression{
				 full= true,
				 op= ?tt2,
				 left= ?tt1,
				 right= #paren{
					   exp= #expression{
						   full= true,
						   op= ?tt5,
						   left= ?tt4,
						   right= ?tt6 } } } }]},
      {"2+(4-1)*",  {error, bad_term}},
      {"2+(4-1)*3", [#expression{
			op= ?nt,
			left= #expression{
				 full= true,
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
					   right= ?tt9 } } }]}
    ].

add_term_test() ->
    Str = "2+(4-1)*3",
    Vs = lists:map(fun(N) -> Str1 = lists:sublist(Str, N),
			     {WT, Term} = cases:waiting_tree(Str1),
			     {Str1, acie_parser:add_term(WT, ?nt, Term)}
		   end,
		   lists:seq(0, length(Str))),
    Es = expected_values(),
    lists:map(fun({E, V}) -> ?assertEqual(E, V) end,
	      lists:zip(Es, Vs)).

