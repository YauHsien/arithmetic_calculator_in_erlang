-module(formula_lex1).
-compile(export_all).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/lex.hrl").

-record(state, { raw_input = "" :: string(),
		 terms = [] :: [#term{}],
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


handle_cast({new_string, String}, #state{ raw_input= PrevString, terms= PrevTerms }= State) ->
    {Loc, String1} = appended_input(PrevString, String),
    Terms = append_diff(PrevTerms, Loc, String1),
    {noreply, State#state{ raw_input= String, terms= Terms }};
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

-spec appended_input(PrevString :: string(), NewString :: string()) -> {Loc :: integer(), DiffString :: string()}.
appended_input("", String) ->
    {1, String};
appended_input(PrevString, String) ->
    case drop_common_leading(PrevString, String) of
	{Common, "", String1} ->
	    {length(Common) + 1, String1};
	{Common, PrevString1, String1} ->
	    Len = length(PrevString1),
	    {length(Common) + 1, lists:append(string:chars($<, Len), String1)}
    end.

-spec append_diff(Terms :: [#term{}], Loc :: integer(), DiffString :: string()) -> Terms1 :: [#term{}].
append_diff([]= Terms, 1, String) ->
    append_diff1(1, String, [start]);
append_diff(Terms, Loc, String) ->
    append_diff1(Loc, String, lists:reverse(Terms)).

append_diff1(_Loc, "", [start]) ->
    [];
append_diff1(_Loc, "", Acc) ->
    lists:reverse(Acc);
append_diff1(Loc, [Char|String], [Term|Terms]) ->
    Char1 = cc(Char),
    Loc1 = case Char1 of
	       backspace -> Loc;
	       _ -> Loc + 1
	   end,
    Terms1 = case meet(Term, Loc, Char1) of
		 empty -> Terms;
		 {#term{}= Term2, #term{}= Term3} -> [Term3,Term2|Terms];
		 Term2 -> [Term2|Terms]
	     end,
    append_diff1(Loc1, String, Terms1).


-spec meet(Term :: #term{} | term(), Loc :: integer(), Char :: char()) -> start | empty | #term{} | {#term{}, #term{}}.

meet(start, _Loc, backspace) ->
    start;
meet(#term{ type= numeral, value= N }= Term, _Loc, backspace) ->
    case lists:droplast(N) of
	"" -> empty;
	N1 -> update_term(Term, N1)
    end;
meet(_, _Loc, backspace) ->
    empty;

meet(start, Loc, lparan) ->
    build_term(lparan, Loc);
meet(Term, Loc, lparan) ->
    {Term, build_term(lparan, Loc)};

meet(start, Loc, rparan) ->
    build_term(rparan, Loc);
meet(Term, Loc, rparan) ->
    {Term, build_term(rapaan, Loc)};

meet(start, Loc, {digit, N}) ->
    build_term(numeral, Loc, N);
meet(#term{ type= numeral }= Term, _Loc, {digit, N}) ->
    append_term(Term, N);
meet(#term{}= Term, Loc, {digit, N}) ->
    {Term, build_term(numeral, Loc, N)};

meet(start, Loc, {op_add, Op}) ->
    build_term(op_add, Loc, Op);
meet(#term{}= Term, Loc, {op_add, Op}) ->
    {Term, build_term(op_add, Loc, Op)};

meet(start, Loc, {op_mul, Op}) ->
    {Loc + 1, #term{ type= op_mul, loc= Loc, value= Op }};
meet(#term{}= Term, Loc, {op_mul, Op}) ->
    {Term, build_term(op_mul, Loc, Op)};

meet(_Term, _Loc, empty) ->
    empty.


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

build_term(Type, Loc) ->
    #term{ type= Type, loc= Loc }.

build_term(Type, Loc, Value) ->
    #term{ type= Type, loc= Loc, value= Value}.

append_term(#term{ value= Value }= Term, Value1) ->
    Term#term{ value= lists:append(Value, Value1) }.

update_term(#term{}= Term, Value) ->
    Term#term{ value= Value }.

-spec cc(char()) -> atom() | {atom(), term()}.
%% cc: convert character; build term
cc($<) -> backspace;
cc($() -> lparam;
cc($)) -> rparam;
cc($+) -> {op_add, "+"};
cc($-) -> {op_add, "-"};
cc($*) -> {op_mul, "*"};
cc($/) -> {op_mul, "/"};
cc($0) -> {digit, "0"};
cc($1) -> {digit, "1"};
cc($2) -> {digit, "2"};
cc($3) -> {digit, "3"};
cc($4) -> {digit, "4"};
cc($5) -> {digit, "5"};
cc($6) -> {digit, "6"};
cc($7) -> {digit, "7"};
cc($8) -> {digit, "8"};
cc($9) -> {digit, "9"};
cc(_) -> empty.
