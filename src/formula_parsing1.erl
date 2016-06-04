-module(formula_parsing1).
-compile(export_all).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/parsing.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    self() ! {self(), config, Args},
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({Pid, config, _Args}, State) when Pid == self() ->
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

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

