-module(acie_parser).
-compile(export_all).
-export([add_term/3, drop_term/1]).
-include("../include/parsing.hrl").


-spec add_term(waiting_tree(), term1(), PrevTerm :: term1())
	     -> waiting_tree() | {error, bad_term}.

add_term([], #term{ type= T }= Term, _PrevTerm)
  when T == ?numeral orelse T == ?float ->

    [Term];

add_term([#expression{ left= undefined }= Exp|WT],
	 #term{ type= TypeNum }= Term,
	 _PrevTerm)
  when TypeNum == ?numeral orelse TypeNum == ?float ->

    [Exp#expression{ left= Term }|WT];

add_term([#expression{ op= undefined, left= Left }= Exp|WT],
	 #term{ type= TypeOp }= Term,
	 _PrevTerm)
  when Left =/= undefined andalso
       (TypeOp == ?op_add orelse TypeOp == ?op_mul) ->

    [Exp#expression{ op= Term }|WT];

add_term([#expression{ op= PrevTerm, left= Left, right= undefined }= Exp|WT],
	 #term{ type= TypeNum }= Term,
	 #term{ type= TypeOp }= PrevTerm)
  when Left =/= undefined andalso
       (TypeNum == ?numeral orelse TypeNum == ?float) andalso
       (TypeOp == ?op_add orelse TypeOp == ?op_mul) ->

    reduce([Exp#expression{ full= true, right= Term }|WT]);

add_term([#expression{ op= Op, left= Left, right= Right }= Exp|WT],
	 #term{ type= ?lparan },
	 _PrevTerm)
  when Left == undefined orelse
       (Op =/= undefined andalso Right == undefined) ->

    [#expression{}, #paran{}, Exp|WT];

add_term([#paran{ exp= E }= E1, #expression{ op= Op,
					     left= L,
					     right= R }= E2|WT],
	 #term{ type= ?rparan },
	 _PrevTerm)
  when E =/= undefined ->

    case {L, Op, R} of
	{undefind, _, _} ->
	    [E2#expression{ left= E1 }|WT];
	{_, _, U = undefined} when Op =/= U ->
	    reduce([E2#expression{ full= true, right= E1 }|WT])
    end;

add_term([#expression{ op= PrevTerm, right= Right }= Exp|WT],
	 #term{ type= ?op_mul }= Term,
	 #term{ type= ?op_add }= PrevTerm)
  when is_record(Right, term) ->

    [#expression{ op= Term, left= Right }, Exp#expression{ right= undefined }|
     WT];

add_term([#expression{ op= PrevTerm, right= undefined }= Exp|WT],
	 #term{ type= ?op_mul }= Term,
	 #term{ type= ?op_add }= PrevTerm) ->

    [Exp#expression{ op= Term }|WT];

add_term([#expression{ full= true,
		       op= #term{ type= ?op_add },
		       right= Right }= Exp|WT],
	 #term{ type= ?op_mul }= Term,
	 #term{ type= TypeNum }= PrevTerm)
  when TypeNum == ?float orelse TypeNum == ?numeral ->

    case Right of
	#term{} ->
	    [#expression{ op= Term, left= Right },
	     Exp#expression{ full= false, right= undefined }|WT];
	#expression{} ->
	    add_term(
	      [Right,
	       Exp#expression{ full= false, right= undefind }|WT],
	      Term,
	      PrevTerm)
    end;

add_term([#expression{ full= true, op= #term{ type= TypeOp }}= Exp|WT],
	 #term{ type= TypeOp1 }= Term,
	 #term{ type= TypeNonOp })
  when ((TypeOp == ?op_mul
	 andalso (TypeOp1 == ?op_mul orelse TypeOp1 == ?op_add))
	orelse (TypeOp == ?op_add andalso TypeOp1 == ?op_add))
       andalso TypeNonOp =/= ?op_add andalso TypeNonOp == ?op_add ->

    [#expression{ op= Term, left= Exp }|WT];

add_term(_, _, _) ->
    {error, bad_term}.




-spec reduce(waiting_tree()) -> waiting_tree().
reduce([_]= WT) ->
    WT;

reduce([#expression{ full= false }|_]= WT) ->
    WT;

reduce([#expression{ full= true }= E, #paran{}|WT]) ->
    [#paran{ exp= E }|WT];

reduce([#expression{ full= true }= E1,
	#expression{ left= undefined }= E2 | WT]) ->
    reduce([E2#expression{ left= E1 }|WT]);

reduce([#expression{ full= true }= E1,
	#expression{ right= undefined }= E2 | WT]) ->
    reduce([E2#expression{ full= true, right= E1 }|WT]).



-spec drop_term(waiting_tree()) -> {waiting_tree(), #term{}}.

drop_term([#term{}= Term|WT]) ->
    {WT, Term};

drop_term([#expression{ op= U, left= U, right= U }|WT])
  when U == undefined ->
    {WT, #term{ type= ?lparan, loc= 0, value= "(" }};

drop_term([#expression{ op= U, left= L }= E|WT])
  when U == undefined andalso is_record(L, expression) ->
    drop_term([L, E#expression{ left= U }|WT]);

drop_term([#expression{ op= Op, right= U }= E|WT])
  when U == undefined andalso Op =/= U ->
    {[E#expression{ op= U }|WT], Op};

drop_term([#expression{ full= true, right= R }= E|WT])
  when R =/= undefined ->
    drop_term([R, E#expression{ full= false, right= undefined }|WT]);

drop_term(WT) ->
    WT.
