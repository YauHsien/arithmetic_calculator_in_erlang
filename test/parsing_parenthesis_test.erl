-module(parsing_parenthesis_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

-define(lp, #term{ type= ?lparen, loc= 999, value= "(" }).
-define(rp, #term{ type= ?rparen, loc= 888, value= ")" }).

expected_values($() ->
    [ {"",          [#paren{}]},
      {"2",         {error, bad_term}},
      {"2+",        [#paren{},
		     #expression{ op= ?tt2, left= ?tt1 }]},
      {"2+(",       [#paren{}, #paren{},
		     #expression{ op= ?tt2, left= ?tt1 }]},
      {"2+(4",      {error, bad_term}},
      {"2+(4-",     [#paren{},
		     #expression{ op= ?tt5, left= ?tt4 },
		     #paren{},
		     #expression{ op= ?tt2, left= ?tt1}]},
      {"2+(4-1",    {error, bad_term}},
      {"2+(4-1)",   {error, bad_term}},
      {"2+(4-1)*",  [#paren{},
		     #expression{
			op= ?tt8,
			left= #paren{
				 exp = #expression{
					  full= true,
					  op= ?tt5,
					  left= ?tt4,
					  right= ?tt6 }}},
		     #expression{
			op= ?tt2,
			left= ?tt1,
			right= undefined }]},
      {"2+(4-1)*3", {error, bad_term}}
    ];
expected_values($)) ->
    [ {"",          {error, bad_term}},
      {"2",         {error, bad_term}},
      {"2+",        {error, bad_term}},
      {"2+(",       {error, bad_term}},
      {"2+(4",      {error, bad_term}},
      {"2+(4-",     {error, bad_term}},
      {"2+(4-1",    [#expression{ full= true,
				  op= ?tt2,
				  left= ?tt1,
				  right= #paren{ exp= #expression{ full= true,
								   op= ?tt5,
								   left= ?tt4,
								   right= ?tt6
								 }}}]},
      {"2+(4-1)",   {error, bad_term}},
      {"2+(4-1)*",  {error, bad_term}},
      {"2+(4-1)*3", {error, bad_term}}
    ].

add_term_lparen_test() ->
    Str = "2+(4-1)*3",
    Vs = lists:map(fun(N) -> Str1 = lists:sublist(Str, N),
			     {WT, Term} = cases:waiting_tree(Str1),
			     {Str1, acie_parser:add_term(WT, ?lp, Term)}
		   end,
		   lists:seq(0, length(Str))),
    Es = expected_values($(),
    lists:map(fun({E, V}) -> ?assertEqual(E, V) end,
	      lists:zip(Es, Vs)).

add_term_rparen_test() ->
    Str = "2+(4-1)*3",
    Vs = lists:map(fun(N) -> Str1 = lists:sublist(Str, N),
			     {WT, Term} = cases:waiting_tree(Str1),
			     {Str1, acie_parser:add_term(WT, ?rp, Term)}
		   end,
		   lists:seq(0, length(Str))),
    Es = expected_values($)),
    lists:map(fun({E, V}) -> ?assertEqual(E, V) end,
	      lists:zip(Es, Vs)).
