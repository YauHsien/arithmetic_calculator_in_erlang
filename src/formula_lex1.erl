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


handle_cast({new_string, String}, #state{ raw_input= PrevString, terms= PrevTerms, send_to= SendTo }= State) ->
    {BackCount, Loc, String1} = appended_input(PrevString, String),
    PrevTerms1 = backspace_terms(PrevTerms, BackCount, SendTo),
    Terms = append_diff(PrevTerms1, Loc, String1, SendTo),
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

-spec appended_input(PrevString :: string(), NewString :: string()) -> {BackCount :: integer(), Loc :: integer(), DiffString :: string()}.
appended_input("", String) ->
    {0, 1, String};
appended_input(PrevString, String) ->
    case drop_common_leading(PrevString, String) of
	{Common, "", String1} ->
	    {0, length(Common) + 1, String1};
	{Common, PrevString1, String1} ->
	    {length(PrevString1), length(Common) + 1, String1}
    end.


-spec backspace_terms(Terms :: [#term{}], BackCount :: integer(), SendTo :: pid()) -> Terms1 :: [#term{}].
backspace_terms(Terms, BackCount, _SendTo) when Terms == [] orelse BackCount =< 0 ->
    Terms;
backspace_terms(Terms, BackCount, SendTo) ->
    backspace_terms1(lists:reverse(Terms), BackCount, undefined, SendTo).

backspace_terms1(Acc, 0, update_term, SendTo) ->
    gen_server:cast(SendTo, {update_term, hd(Acc)}),
    lists:reverse(Acc);
backspace_terms1(Acc, 0, _Acc1, _SendTo) ->
    lists:reverse(Acc);
backspace_terms1(Terms, BackCount, _Acc, SendTo) ->
    {Terms1, Acc1} = case meet(Terms, 0, backspace) of
			 empty ->
			     gen_server:cast(SendTo, drop_term),
			     {tl(Terms), undefined};
			 #term{}= Term1 ->
			     {[Term1|tl(Terms)], update_term}
		     end,
    backspace_terms1(Terms1, BackCount - 1, Acc1, SendTo).
		     

-spec append_diff(Terms :: [#term{}], Loc :: integer(), DiffString :: string(), SendTo :: pid()) -> Terms1 :: [#term{}].
append_diff(Terms, Loc, String, SendTo) ->
    append_diff1(Loc, String, lists:reverse(Terms), SendTo).

append_diff1(_Loc, "", [], _SendTo) ->
    [];
append_diff1(_Loc, "", Acc, SendTo) ->
    Terms1 = case Acc of
		 [?sep|Terms] ->
		     Terms;
		 Terms ->
		     gen_server:cast(SendTo, {add_term, hd(Terms)}),
		     Terms
	     end,
    lists:reverse(Terms1);
append_diff1(Loc, [Char|String], Terms, SendTo) ->
    case meet(Terms, Loc, cc(Char)) of
	#term{}= NewTerm ->
	    case Terms of
		Terms1 when Terms1 == [] orelse Terms1 == [?sep] ->
		    append_diff1(Loc + 1, String, [NewTerm], SendTo);
		[?sep|Terms1] ->
io:fwrite("new term: ~w~nterm1: ~w~n", [NewTerm, hd(Terms1)]),
		    case {is_op(NewTerm), is_op(hd(Terms1))} of
			{true, true} ->
			    append_diff1(Loc + 1, String, [NewTerm|tl(Terms1)], SendTo);
			_ ->
			    append_diff1(Loc + 1, String, [NewTerm|Terms1], SendTo)
		    end;
		_ ->
		    gen_server:cast(SendTo, {add_term, hd(Terms)}),
		    append_diff1(Loc + 1, String, [NewTerm|Terms], SendTo)
	    end;
	[Term|_]= Terms1 when Term == ?sep orelse is_record(Term, term) ->
	    append_diff1(Loc + 1, String, Terms1, SendTo);
	?sep= _NewTerm ->
	    case Terms of
		[] -> ok;
		_ -> gen_server:cast(SendTo, {add_term, hd(Terms)})
	    end,
	    append_diff1(Loc + 1, String, [?sep|Terms], SendTo);
	{replace, #term{}= Term1} ->
	    gen_server:cast(SendTo, drop_term),
	    gen_server:cast(SendTo, {add_term, Term1}),
	    Terms1 = [Term1|tl(Terms)],
	    append_diff1(Loc + 1, String, Terms1, SendTo)
    end.


-spec meet(Terms :: terms1(), Loc :: integer(), backspace) -> ?empty | (NewTerm :: term1());
	  (Terms :: terms1(), Loc :: integer(), Char :: char()) -> (NewTerm :: term1()) | (Terms1 :: terms1()) | {replace, #term{}}.

meet([#term{ type= ?numeral, value= N }= Term|_], _Loc, ?backspace) ->
    case lists:droplast(N) of
	"" -> ?empty;
	N1 -> update_term(Term, N1)
    end;
meet(_, _Loc, ?backspace) ->
    ?empty;

meet(_Terms, Loc, ?lparan) ->
    build_term(?lparan, Loc);

meet(_Terms, Loc, ?rparan) ->
    build_term(?rparan, Loc);

meet([#term{ type= ?numeral }= Term|Terms], _Loc, ?point) ->
    [append_term(convert_term(Term, ?float), ".")|Terms];
meet([#term{ type= ?float }= Term|Terms], _Loc, ?point) ->
    [append_term(convert_term(Term, ?not_accepted), ".")|Terms];
meet([#term{ type= ?not_accepted }= Term|Terms], _Loc, ?point) ->
    [append_term(Term, ".")|Terms];
meet(_Terms, Loc, ?point) ->
    build_term(?not_accepted, Loc, ".");

meet([#term{ type= Type }= Term|Terms], _Loc, {?digit, N}) when Type == ?numeral orelse Type == ?float ->
    [append_term(Term, N)|Terms];
meet([#term{ type= ?not_accepted }= Term|Terms], _Loc, {?digit, N}) ->
    [append_term(Term, N)|Terms];
meet(_Term, Loc, {?digit, N}) ->
    build_term(?numeral, Loc, N);

meet([#term{ type= Type }|_], Loc, {Type1, Op}) when (Type == ?op_add orelse Type == ?op_mul) andalso (Type1 == ?op_add orelse Type1 == ?op_mul) ->
    {replace, build_term(Type1, Loc, Op)};

meet(_Terms, Loc, {?op_add, Op}) ->
    build_term(?op_add, Loc, Op);

meet(_Terms, Loc, {?op_mul, Op}) ->
    build_term(?op_mul, Loc, Op);

meet([?sep|_]= Terms, _Loc, ?sep) ->
    Terms;
meet(_Terms, _Loc, ?sep) ->
    ?sep;

meet([#term{ type= ?not_accepted }= Term|Terms], _Loc, {?not_accepted, S}) ->
    [append_term(Term, S)|Terms];
meet(_Terms, Loc, {?not_accepted, S}) ->
    build_term(?not_accepted, Loc, S).


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

convert_term(#term{}= Term, Type) ->
    Term#term{ type= Type }.

is_op(#term{ type= ?op_mul }) -> true;
is_op(#term{ type= ?op_add }) -> true;
is_op(_) -> false.


-spec cc(char()) -> atom() | {atom(), term()}.
%% cc: convert character; build term
cc($<) -> ?backspace;
cc($() -> ?lparan;
cc($)) -> ?rparan;
cc($+) -> {?op_add, "+"};
cc($-) -> {?op_add, "-"};
cc($*) -> {?op_mul, "*"};
cc($/) -> {?op_mul, "/"};
cc($.) -> ?point;
cc($0) -> {?digit, "0"};
cc($1) -> {?digit, "1"};
cc($2) -> {?digit, "2"};
cc($3) -> {?digit, "3"};
cc($4) -> {?digit, "4"};
cc($5) -> {?digit, "5"};
cc($6) -> {?digit, "6"};
cc($7) -> {?digit, "7"};
cc($8) -> {?digit, "8"};
cc($9) -> {?digit, "9"};
cc($\s) -> ?sep;
cc(S) -> {?not_accepted, [S]}.
