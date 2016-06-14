-module(parsing_numeral_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

-define(nt, #term{ type= ?numeral, loc= 3, value= "999" }).
%-define(nt, #term{ type= ?float, loc= 5, value="3.14" }).

expected_values() ->
    [ {"",          [?nt]},
      {"2",         {error, bad_term}},
      {"2+",        [#expression{ full= true,
				  op= ?tt2,
				  left= ?tt1,
				  right= ?nt }]},
      {"2+(",       [#expression{ left= ?nt },
		     #paran{},
		     #expression{ op= ?tt2,
				  left= ?tt1 }]},
      {"2+(4",      {error, bad_term}},
      {"2+(4-",     [#paran{ exp= #expression{ full= true,
					       op= ?tt5,
					       left= ?tt4,
					       right= ?nt } },
		     #expression{ op= ?tt2,
				  left= ?tt1 }]},
      {"2+(4-1",    {error, bad_term}},
      {"2+(4-1)",   {error, bad_term}},
      {"2+(4-1)*",  [#expression{
			full= true,
			op= ?tt2,
			left= ?tt1,
			right= #expression{
				  full= true,
				  op= ?tt8,
				  left= #paran{ exp= #expression{ full= true,
								  op= ?tt5,
								  left= ?tt4,
								  right= ?tt6 } },
				  right= ?nt } }]},
      {"2+(4-1)*3", {error, bad_term}}
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


