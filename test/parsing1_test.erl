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

parsing1_add_term_test() ->

    Exp = undefined,

    Exp1 = #expression{},

    ?assertEqual(Exp1, formula_parsing1:add_term(Exp,
						 #term{
						    type= ?lparan,
						    loc= 1
						   })).


parsing1_drop_term_test() ->

    Exp =
	#expression{
	   type= ?factor,
	   op= "/",
	   left=
	       #expression{
		  type= ?factor,
		  op= "*",
		  left =
		      #expression{
			 type= ?addition,
			 op= "+",
			 left = #term{ type= ?numeral, loc= 2, value= "12" },
			 right = #term{ type= ?numeral, loc= 5, value= "34" }
			},
		  right= #term{ type= ?numeral, loc= 8, value= "56" }
		 },
	   right= #term{ type= ?numeral, loc= 11, value= "78" }
	  },

    Exp1 =
	#expression{
	   type= ?factor,
	   op= "/",
	   left=
	       #expression{
		  type= ?factor,
		  op= "*",
		  left =
		      #expression{
			 type= ?addition,
			 op= "+",
			 left = #term{ type= ?numeral, loc= 2, value= "12" },
			 right = #term{ type= ?numeral, loc= 5, value= "34" }
			},
		  right= #term{ type= ?numeral, loc= 8, value= "56" }
		 },
	   right= undefined
	  },

    Exp2 =
	#expression{
	   type= ?factor,
	   op= "*",
	   left =
	       #expression{
		  type= ?addition,
		  op= "+",
		  left = #term{ type= ?numeral, loc= 2, value= "12" },
		  right = #term{ type= ?numeral, loc= 5, value= "34" }
		 },
	   right= #term{ type= ?numeral, loc= 8, value= "56" }
	  },
    
    ?assertEqual(Exp1, formula_parsing1:drop_term(Exp)),
    ?assertEqual(Exp2, formula_parsing1:drop_term(Exp1)).
