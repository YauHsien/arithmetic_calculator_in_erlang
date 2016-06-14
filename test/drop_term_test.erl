-module(drop_term_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").
-include("../include/test.hrl").

-define(lp, #term{ type= ?lparen, loc= 999, value= "(" }).
-define(rp, #term{ type= ?rparen, loc= 888, value= ")" }).

expected_values() ->
    [ {"2",         cases:waiting_tree("")          ,ok},
      {"2+",        cases:waiting_tree("2")         ,ok},
      {"2+(",       cases:waiting_tree("2+")        ,ok},
      {"2+(4",      cases:waiting_tree("2+(")       ,ok},
      {"2+(4-",     cases:waiting_tree("2+(4")      , ok},
      {"2+(4-1",    cases:waiting_tree("2+(4-")     , ok},
      {"2+(4-1)",   cases:waiting_tree("2+(4-1")    , ok},
      {"2+(4-1)*",  cases:waiting_tree("2+(4-1)")   ,ok},
      {"2+(4-1)*3", cases:waiting_tree("2+(4-1)*")  ,ok}
    ].

cases_test() ->
    Str = "2+(4-1)*3",
    Vs = lists:map(fun(N) -> Str1 = lists:sublist(Str, N),
			     {WT, _Term} = cases:waiting_tree(Str1),
			     {WT1, Term1} = acie_parser:drop_term(WT),
			     {Str1, WT1, ok}
		   end,
		   lists:seq(1, length(Str))),
    Es = lists:map(fun({A, {B, _}, C}) -> {A, B, C} end, expected_values()),
    lists:map(fun({_, bypass}) -> ok;
		 ({E, V}) -> ?assertEqual(E, V) end,
	      lists:zip(Es, Vs)).
