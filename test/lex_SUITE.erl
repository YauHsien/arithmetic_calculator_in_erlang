-module(lex_SUITE).
-include_lib("common_test/include/ct.hrl").

all() ->
    [lex1_lex2_testcase, lex1_lex2_testcase2].

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
%% Eshell V7.2  (abort with ^G)
%% 1>     {ok, Lex2} = formula_lex2:start_link(empty),
%% 1>     {ok, Lex1} = formula_lex1:start_link(Lex2),
%% 1>     gen_server:cast(Lex1, {new_string, "(12+34)*56/78"}).
%% {add_term,{term,lparan,1,undefined}} received
%% {add_term,{term,numeral,2,[49,50]}} received
%% {add_term,{term,op_add,4,[43]}} received
%% {add_term,{term,numeral,5,[51,52]}} received
%% {add_term,{term,rparan,7,undefined}} received
%% {add_term,{term,op_mul,8,[42]}} received
%% {add_term,{term,numeral,9,[53,54]}} received
%% {add_term,{term,op_mul,11,[47]}} received
%% {add_term,{term,numeral,12,[55,56]}} received
%% ok
