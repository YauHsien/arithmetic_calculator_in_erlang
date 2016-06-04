-module(parsing1_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

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

%% parsing1_add_term_test() ->

%%     Exp = undefined,

%%     Term1 = #term{ type= ?lparan, loc= 1 },
%%     Exp1 = #expression{},

%%     Term2 = #term{ type= ?numeral, loc= 2, value= "12" },
%%     Exp2 = #expression{ left= Term2 },

%%     Term3 = #term{ type= ?op_add, loc= 4, value= "+" },
%%     Exp3 = #expression{ type= ?op_add, left= Term2 },

%%     Term4 = #term{ type= ?numeral, loc= 5, value= "34" },
%%     Exp4 = #expression{ type= ?op_add, left= Term2, right= Term4 },

%%     Term5 = #term{ type= ?rparan, loc= 7 },
%%     Exp5 = #expression{ left= Exp4 },

%%     Term6 = #term{ type= ?op_mul, loc= 8, value= "/" },
%%     Exp6 = #expression{ type= ?op_div, left= Exp4 },

%%     Term7 = #term{ type= ?numeral, loc= 9, value= "56" },
%%     Exp7 = #expression{ type= ?op_div, left= Exp4, right= Term7 },

%%     Term8 = #term{ type= ?op_mul, loc= 11, value= "*" },
%%     Exp8 = #expression{ type= ?op_mul, left= Exp7 },

%%     Term9 = #term{ type= ?numeral, loc= 12, value= "78" },
%%     Exp9 = #expression{ type= ?op_mul, left= Exp7, right= Term9 },

%%     ?assertEqual(Exp1, formula_parsing1:add_term(Exp, Term1)),
%%     ?assertEqual(Exp2, formula_parsing1:add_term(Exp1, Term2)),
%%     ?assertEqual(Exp3, formula_parsing1:add_term(Exp2, Term3)),
%%     ?assertEqual(Exp4, formula_parsing1:add_term(Exp3, Term4)),
%%     ?assertEqual(Exp5, formula_parsing1:add_term(Exp4, Term5)),
%%     ?assertEqual(Exp6, formula_parsing1:add_term(Exp5, Term6)),
%%     ?assertEqual(Exp7, formula_parsing1:add_term(Exp6, Term7)),
%%     ?assertEqual(Exp8, formula_parsing1:add_term(Exp7, Term8)),
%%     ?assertEqual(Exp9, formula_parsing1:add_term(Exp8, Term9)).


%% parsing1_drop_term_test() ->

%%     Exp =
%% 	#expression{
%% 	   type= ?factor,
%% 	   op= "/",
%% 	   left=
%% 	       #expression{
%% 		  type= ?factor,
%% 		  op= "*",
%% 		  left =
%% 		      #expression{
%% 			 type= ?addition,
%% 			 op= "+",
%% 			 left = #term{ type= ?numeral, loc= 2, value= "12" },
%% 			 right = #term{ type= ?numeral, loc= 5, value= "34" }
%% 			},
%% 		  right= #term{ type= ?numeral, loc= 8, value= "56" }
%% 		 },
%% 	   right= #term{ type= ?numeral, loc= 11, value= "78" }
%% 	  },

%%     Exp1 =
%% 	#expression{
%% 	   type= ?factor,
%% 	   op= "/",
%% 	   left=
%% 	       #expression{
%% 		  type= ?factor,
%% 		  op= "*",
%% 		  left =
%% 		      #expression{
%% 			 type= ?addition,
%% 			 op= "+",
%% 			 left = #term{ type= ?numeral, loc= 2, value= "12" },
%% 			 right = #term{ type= ?numeral, loc= 5, value= "34" }
%% 			},
%% 		  right= #term{ type= ?numeral, loc= 8, value= "56" }
%% 		 },
%% 	   right= undefined
%% 	  },

%%     Exp2 =
%% 	#expression{
%% 	   type= ?factor,
%% 	   op= "*",
%% 	   left =
%% 	       #expression{
%% 		  type= ?addition,
%% 		  op= "+",
%% 		  left = #term{ type= ?numeral, loc= 2, value= "12" },
%% 		  right = #term{ type= ?numeral, loc= 5, value= "34" }
%% 		 },
%% 	   right= #term{ type= ?numeral, loc= 8, value= "56" }
%% 	  },
    
%%     ?assertEqual(Exp1, formula_parsing1:drop_term(Exp)),
%%     ?assertEqual(Exp2, formula_parsing1:drop_term(Exp1)).

%% parsing1_add_drop_test() ->

%%     Exp =
%% 	#expression{
%% 	   type= ?op_sub,
%% 	   left =
%% 	       #expression{
%% 		  type= ?op_mul,
%% 		  left= #term{ type= ?numeral, loc= 2, value= "12" },
%% 		  right=
%% 		      #expression{
%% 			 type= ?op_add,
%% 			 left= #term{ type= ?float, loc= 7, value= "34.25" },
%% 			 right= #term{ type= ?numeral, loc = 10, value= "56" }
%% 			}
%% 		 },
%% 	   right= #term{ type= ?float, loc= 13, value= "7.5" }
%% 	  },

%%     Exp1 = Exp#expression{
%% 	     type= ?op_div,
%% 	     right= #term{ type= ?numeral, loc= 14, value= "78" }
%% 	    },

%%     ?assertEqual(Exp1, formula_parsing1:add_term(
%% 			 formula_parsing1:add_term(
%% 			   formula_parsing1:drop_term(
%% 			     formula_parsing1:drop_term(
%% 			       Exp
%% 			      )
%% 			    ),
%% 			   #term{ type= ?op_mul, loc= 12, value= "/" }
%% 			  ),
%% 			 #term{ type= ?numeral, loc= 14, value= "78" }
%% 			)).

%% parsing1_parse_test() ->

%%     Exp = undefined,

%%     Terms =
%% 	[ {add_term, #term{ type= ?numeral, loc= 2, value= "12" }},
%% 	  {add_term, #term{ type= ?op_mul, loc= 3, value= "*" }},
%% 	  {add_term, #term{ type= ?lparan, loc=5, value= "(" }},
%% 	  {add_term, #term{ type= ?float, loc= 7, value= "3.4" }},
%% 	  {add_term, #term{ type= ?op_mul, loc= 10, value= "+" }},
%% 	  {add_term, #term{ type= ?numeral, loc= 10, value= "56" }},
%% 	  {add_term, #term{ type= ?rparan, loc= 11, value= ")" }},
%% 	  {add_term, #term{ type= ?op_add, loc= 12, value= "+" }},
%% 	  {add_term, #term{ type= ?numeral, loc= 13, value= "78" }},
%% 	  drop_term,
%% 	  drop_term,
%% 	  {add_term, #term{ type= ?op_add, loc= 13, value= "/" }},
%% 	  {add_term, #term{ type= ?numeral, loc= 14, value= "78" }}
%% 	],

%%     Exp1 = 
%% 	#expression{
%% 	   type= ?op_div,
%% 	   left =
%% 	       #expression{
%% 		  type= ?op_mul,
%% 		  left= #term{ type= ?numeral, loc= 2, value= "12" },
%% 		  right=
%% 		      #expression{
%% 			 type= ?op_add,
%% 			 left= #term{ type= ?float, loc= 7, value= "3.4" },
%% 			 right= #term{ type= ?numeral, loc = 10, value= "56" }
%% 			}
%% 		 },
%% 	   right= #term{ type= ?numeral, loc= 14, value= "78" }
%% 	  },
%% io:fwrite("~p~n", [formula_parsing1:parse(Exp, Terms)]),
%%     ?assertEqual(Exp1, formula_parsing1:parse(Exp, Terms)).
