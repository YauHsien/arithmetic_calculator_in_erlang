-module(lex_SUITE).
-include_lib("common_test/include/ct.hrl").

all() ->
    [lex1_lex2_testcase].

lex1_lex2_testcase() ->
    {ok, Lex2} = formula_lex2:start_link(empty),
    {ok, Lex1} = formula_lex1:start_link(Lex2),
    gen_server:cast(Lex1, {new_string, "hello"}).
%Eshell V7.2  (abort with ^G)
%1> {ok, Lex2} = formula_lex2:start_link(empty),
%1>     {ok, Lex1} = formula_lex1:start_link(Lex2),
%1>     gen_server:cast(Lex1, {new_string, "hello"}).
%stop received
%ok

lex1_lex2_testcase2() ->
    {ok, Lex2} = formula_lex2:start_link(empty),
    {ok, Lex1} = formula_lex1:start_link(Lex2),
    gen_server:cast(Lex1, {new_string, "(12+34)*56/78"}).
%3>     gen_server:cast(Lex1, {new_string, "(12+34)*56/78"}).
%lparan received
%ok
%{digit,1} received
%{digit,2} received
%{op_add,plus} received
%{digit,3} received
%{digit,4} received
%rparan received
%{op_mul,multiply} received
%{digit,5} received
%{digit,6} received
%{op_mul,devide} received
%{digit,7} received
%{digit,8} received
%stop received
