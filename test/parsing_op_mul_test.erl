-module(parsing_op_mul_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

-define(nt, #term{ type= ?op_mul, loc= 3, value= "*" }).
%-define(nt, #term{ type= ?op_mul, loc= 5, value= "/" }).

expected_values() ->
    [ {"",          {error, bad_term}},
      {"2",         [#expression{ op= ?nt,
				  left= ?tt1 }]},
      {"2+",        [#expression{ op= ?nt,
				  left= ?tt1 }]},
      {"2+(",       {error, bad_term}},
      {"2+(4",      [#expression{ op= ?nt,
				  left= ?tt4 },
		     #paran{},
		     #expression{ op= ?tt2,
				  left= ?tt1 }]},
      {"2+(4-",     [#expression{ op= ?nt,
				  left= ?tt4 },
		     #paran{},
		     #expression{ op= ?tt2,
				  left= ?tt1}]},
      {"2+(4-1",    [#expression{ op= ?nt,
				  left= ?tt6 },
		     #expression{ op= ?tt5,
				  left= ?tt4 },
		     #paran{},
		     #expression{ op= ?tt2,
				  left= ?tt1 }]},
      {"2+(4-1)",   [#expression{
			op= ?nt,
			left= #paran{
				 exp= #expression{
					 full= true,
					 op= ?tt5,
					 left= ?tt4,
					 right= ?tt6 }}},
		     #expression{
			op= ?tt2,
			left= ?tt1 }]},
      {"2+(4-1)*",  {error, bad_term}},
      {"2+(4-1)*3", [#expression{
			op= ?nt,
			left= #expression{
				 full= true,
				 op= ?tt8,
				 left= #paran{
					  exp= #expression{
						  full= true,
						  op= ?tt5,
						  left= ?tt4,
						  right= ?tt6 }},
				 right= ?tt9 } },
		     #expression{ op= ?tt2,
				  left= ?tt1 }]}
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
