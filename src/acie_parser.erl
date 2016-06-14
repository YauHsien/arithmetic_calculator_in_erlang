-module(acie_parser).
-compile(export_all).
-export([add_term/3, drop_term/1]).
-include("../include/parsing.hrl").


-spec add_term(waiting_tree(), term1(), PrevTerm :: term1())
	     -> waiting_tree() | {error, bad_term}.

add_term(WT, #term{ type= T }= Term, _PrevTerm)
  when T == ?numeral orelse T == ?float ->

    case WT of
	[] ->
	    [Term];

	[#paren{}|_] ->
	    [#expression{ left= Term }|WT];

	[#expression{ left= undefined }= E|WT1] ->
	    [E#expression{ left= Term }|WT1];

	[#expression{ op= Op, right= U }= E|WT1]
	  when U == undefined andalso Op =/= U ->
	    reduce([E#expression{ full= true, right= Term }|WT1]);

	_ ->
	    {error, bad_term}
    end;

add_term([#expression{ left= undefined }= Exp|WT],
	 #term{ type= TypeNum }= Term,
	 _PrevTerm)
  when TypeNum == ?numeral orelse TypeNum == ?float ->

    [Exp#expression{ left= Term }|WT];

add_term([#term{ type= TypeNum }= Term], #term{ type= TypeOp }= Term1, Term)
  when (TypeNum == ?numeral orelse TypeNum == ?float) orelse
       (TypeOp == ?op_add orelse TypeOp == ?op_mul) ->

    add_term([#expression{ left= Term }], Term1, Term);

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

add_term(WT, #term{ type= ?lparen }, _PrevTerm) ->

    case WT of
	[] ->
	    [#paren{}];

	[#expression{ left= undefined }|_] ->
	    [#paren{}|WT];

	[#expression{ op= Op, right= U }|_]
	  when U == undefined andalso Op =/= U ->
	    [#paren{}|WT];

	[#paren{}|_] ->
	    [#paren{}|WT];

	_ ->
	    {error, bad_term}
    end;

add_term([#expression{ full= true }= E, #paren{}|WT], #term{ type= ?rparen },
	 _PrevTerm) ->

    reduce([#paren{ exp= E }|WT]);

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
	 #term{ type= TypeNonOp }= PrevTerm)
  when TypeNonOp =/= ?op_mul andalso TypeNonOp =/= ?op_add ->

    case Right of
	Case when is_record(Case, term) orelse is_record(Case, paren)  ->
	    [#expression{ op= Term, left= Right },
	     Exp#expression{ full= false, right= undefined }|WT];
	#expression{} ->
	    add_term(
	      [Right,
	       Exp#expression{ full= false, right= undefined }|WT],
	      Term,
	      PrevTerm)
    end;

add_term([#expression{ full= true, op= #term{ type= TypeOp }}= Exp|WT],
	 #term{ type= TypeOp1 }= Term,
	 #term{ type= TypeNonOp })
  when ((TypeOp == ?op_mul
	 andalso (TypeOp1 == ?op_mul orelse TypeOp1 == ?op_add))
	orelse (TypeOp == ?op_add andalso TypeOp1 == ?op_add))
       andalso TypeNonOp =/= ?op_add ->

    [#expression{ op= Term, left= Exp }|WT];

add_term(_, _, _) ->
    {error, bad_term}.




-spec reduce(waiting_tree()) -> waiting_tree().

reduce([#expression{ full= false }|_]= WT) ->
    WT;

reduce([#expression{ full= true }= E1,
	#expression{ left= undefined }= E2 | WT]) ->
    reduce([E2#expression{ left= E1 }|WT]);

reduce([#expression{ full= true }= E1,
	#expression{ right= undefined }= E2 | WT]) ->
    reduce([E2#expression{ full= true, right= E1 }|WT]);

reduce([#paren{ exp= E }= E1, E2|WT]= WT1) ->

    case {E, E2} of
	{undefined, _} ->
	    WT1;

	{_, #expression{ left= undefined }} ->
	    [E2#expression{ left= E1 }|WT];

	{_, #expression{ op= Op, right= U }}
	  when U == undefined andalso Op =/= U ->
	    [E2#expression{ full= true, right= E1 }|WT]
    end;

reduce(WT) ->
    WT.





-spec drop_term(waiting_tree()) -> {waiting_tree(), #term{}}.

drop_term([#term{}= Term|WT]) ->
    {WT, Term};

drop_term([#expression{ op= U, left= U, right= U }|WT])
  when U == undefined ->
    {WT, #term{ type= ?lparen, loc= 0, value= "(" }};

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
