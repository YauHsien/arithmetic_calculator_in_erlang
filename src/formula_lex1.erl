-module(formula_lex1).
-compile(export_all).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-record(state, { prev_string = "" :: string(),
		 send_to :: pid()
	       }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(SendTo) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SendTo], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    self() ! {self(), config, Args},
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({new_string, String}, #state{ prev_string= PrevString }= State) ->
    String1 = appended_input(PrevString, String),
    gen_server:cast(self(), {emit_characters, String1}),
    State1 = State#state{ prev_string= String },
    {noreply, State1};
handle_cast({emit_characters, String}, #state{ send_to= SendTo }= State) ->
    emit_characters(SendTo, String),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Pid, config, Args}, State) when Pid == self() ->
    [SendTo] = Args,
    {noreply, State#state{ send_to= SendTo }};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

appended_input("", String) ->
    String;
appended_input(PrevString, String) ->
    case drop_common_leading(PrevString, String) of
	{_Common, "", String1} ->
	    String1;
	{_Common, PrevString1, String1} ->
	    Len = length(PrevString1),
	    lists:append(string:chars($<, Len), String1)
    end.

-spec drop_common_leading(string(), string()) -> {Common :: string(), String1 :: string(), String2 :: string()}.
drop_common_leading(String1, String2) ->
    drop_common_leading(String1, String2, _Common= "").

drop_common_leading([X|String1], [X|String2], Common) ->
    drop_common_leading(String1, String2, [X|Common]);
drop_common_leading(String1, String2, Common) ->
    {lists:reverse(Common), String1, String2}.

-spec emit_characters(pid(), string()) -> ok | {error, string()}.
emit_characters(Pid, "") ->
    gen_server:cast(Pid, stop);
emit_characters(Pid, [Char|String]) ->
    case cc(Char) of
	drop -> ok;
	Char1 -> gen_server:cast(Pid, Char1)
    end,
    emit_characters(Pid, String).

-spec cc(char()) -> char() | backspace.
%% cc: convert character
cc($<) -> backspace;
cc($() -> lparan;
cc($)) -> rparan;
cc($+) -> {op_add, plus};
cc($-) -> {op_add, minus};
cc($*) -> {op_mul, multiply};
cc($/) -> {op_mul, devide};
cc($0) -> {digit, 0};
cc($1) -> {digit, 1};
cc($2) -> {digit, 2};
cc($3) -> {digit, 3};
cc($4) -> {digit, 4};
cc($5) -> {digit, 5};
cc($6) -> {digit, 6};
cc($7) -> {digit, 7};
cc($8) -> {digit, 8};
cc($9) -> {digit, 9};
cc(_) -> drop.
    


    
