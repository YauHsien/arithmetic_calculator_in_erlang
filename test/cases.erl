-module(cases).
-compile(export_all).
-include("../include/parsing.hrl").
-include("../include/lex.hrl").
-include("../include/test.hrl").


-spec waiting_tree(
	string() | integer() %% may be part of string "2+(4-1)*3"
       ) -> {waiting_tree(), LastTerm :: #term{}} | {error, bad_arg}.

waiting_tree(Str) when is_list(Str) ->
    N = length(Str),
    case lists:sublist("2+(4-1)*3", N) of
	Str ->
	    waiting_tree(N);
	_ ->
	    {error, bad_arg}
    end;

waiting_tree(0) -> % ""
    {[],
     undefined};

waiting_tree(1) -> % "2"
    {[?tt1],
     ?tt1};

waiting_tree(2) -> % "2+"
    {[#expression{ op= ?tt2, left= ?tt1 }],
     ?tt2};

waiting_tree(3) -> % "2+("
    {[#expression{},
      #paran{},
      #expression{ op= ?tt2, left= ?tt1 }],
     ?tt3};

waiting_tree(4) -> % "2+(4"
    {[#expression{ left= ?tt4 },
      #paran{},
      #expression{ op= ?tt2, left= ?tt1 }],
     ?tt4};

waiting_tree(5) -> % "2+(4-"
    {[#expression{ op= ?tt5, left= ?tt4 },
      #paran{},
      #expression{ op= ?tt2, left= ?tt1 }],
     ?tt5};

waiting_tree(6) -> % "2+(4-1"
    {[#expression{ full= true, op= ?tt5, left= ?tt4, right= ?tt6 },
      #paran{},
      #expression{ op= ?tt2, left= ?tt1 }],
     ?tt6};

waiting_tree(7) -> % "2+(4-1)"
    {[#expression{ full= true,
		   op= ?tt2,
		   left= ?tt1,
		   right= #paran{ exp= #expression{ full= true,
						    op= ?tt5,
						    left= ?tt4,
						    right= ?tt6 } }}],
     ?tt7};

waiting_tree(8) -> % "2+(4-1)*"
    {[#expression{ op= ?tt8,
		   left= #paran{ exp= #expression{ full= true,
						   op= ?tt5,
						   left= ?tt4,
						   right= ?tt6 } }},
      #expression{ op= ?tt2,
		   left= ?tt1 }],
     ?tt8};

waiting_tree(9) -> % "2+(4-1)*3"
    {[#expression{ full= true,
		   op= ?tt2,
		   left= ?tt1,
		   right= #expression{ full= true,
				       op= ?tt8,
				       left= #paran{ exp= #expression{
							     full= true,
							     op= ?tt5,
							     left= ?tt4,
							     right= ?tt6 }}}}],
     ?tt9}.
