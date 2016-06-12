-module(acie_parser).
-compile(export_all).
-export([add_term/3]).
-include("../include/parsing.hrl").


-spec add_term(waiting_tree(), term1(), PrevTerm :: term1())
	     -> waiting_tree() | {error, bad_term}.

add_term([], #term{}= Term, PrevTerm) ->
    add_term([#expression{}], Term, PrevTerm);

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

    [#expression{}, Exp|WT];

add_term([#expression{ full= true }= Exp|WT],
	 #term{ type= ?rparan },
	 _PrevTerm) ->

    [#expression{ left= Exp }|WT];

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

reduce([#expression{ full= true }= E1,
	#expression{ left= undefined }= E2 | WT]) ->
    reduce([E2#expression{ left= E1 }|WT]);

reduce([#expression{ full= true }= E1,
	#expression{ right= undefined }= E2 | WT]) ->
    reduce([E2#expression{ right= E1 }|WT]).

