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
    gen_server:cast(Lex1, {new_string, "(12+34)*56/78"}),
    gen_server:cast(Lex1, {new_string, "(12+34)*56-78"}).
%% {add_term,{term,lparan,1,undefined}} received
%% {add_term,{term,numeral,2,[49,50]}} received
%% {add_term,{term,op_add,4,[43]}} received
%% ok
%% {add_term,{term,numeral,5,[51,52]}} received
%% 2> {add_term,{term,rparan,7,undefined}} received
%% 2> {add_term,{term,op_mul,8,[42]}} received
%% 2> {add_term,{term,numeral,9,[53,54]}} received
%% 2> {add_term,{term,op_mul,11,[47]}} received
%% 2> {add_term,{term,numeral,12,[55,56]}} received
%% 2> drop_term received
%% 2> drop_term received
%% 2> {add_term,{term,op_add,11,[45]}} received
%% 2> {add_term,{term,numeral,12,[55,56]}} received

lex1_lex2_testcase3() ->
    {ok, Lex2} = formula_lex2:start_link(empty),
    {ok, Lex1} = formula_lex1:start_link(Lex2),
    gen_server:cast(Lex1, {new_string, "( a 1.2 +34)*56/78"}).
%% {add_term,{term,lparan,1,undefined}} received
%% {add_term,{term,not_accepted,3,[97]}} received
%% {add_term,{term,float,5,[49,46,50]}} received
%% {add_term,{term,op_add,9,[43]}} received
%% ok
%% {add_term,{term,numeral,10,[51,52]}} received
%% 2> {add_term,{term,rparan,12,undefined}} received
%% 2> {add_term,{term,op_mul,13,[42]}} received
%% 2> {add_term,{term,numeral,14,[53,54]}} received
%% 2> {add_term,{term,op_mul,16,[47]}} received
%% 2> {add_term,{term,numeral,17,[55,56]}} received
