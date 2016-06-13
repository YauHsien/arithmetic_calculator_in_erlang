-module(test_util).
-export([is_waiting_tree/1]).
-include("../include/lex.hrl").
-include("../include/parsing.hrl").


-spec is_waiting_tree(waiting_tree()) -> boolean().
is_waiting_tree([]) ->
    true;
is_waiting_tree([TermOrExp]) ->
    is_term(TermOrExp) orelse is_expression(TermOrExp);
is_waiting_tree(WT) when is_list(WT) ->
    lists:foldl(fun(_, false) ->
			false;
		   (#expression{ full= false }= Exp, _) ->
			is_expression(Exp);
		   (_, _) ->
			false
		end, true, WT);
is_waiting_tree(_) ->
    false.


is_term(#term{ type= ?numeral }) ->
    true;
is_term(#term{ type= ?float }) ->
    true;
is_term(_) ->
    false.


is_expression(#expression{ full= false, op= U, left= U, right= U })
  when U == undefined ->
    true;

is_expression(#expression{ full= false, op= U, left= L, right= U })
  when U == undefined andalso L =/= U ->
    true;

is_expression(#expression{ full= false, op= Op, left= L, right= U })
  when U == undefined andalso Op =/= U andalso L =/= U ->
    true;

is_expression(#expression{ full= true, op= Op, left= L, right= R })
  when Op =/= undefined andalso L =/= undefined andalso R =/= undefined ->
    true;

is_expression(_) ->
    false.

