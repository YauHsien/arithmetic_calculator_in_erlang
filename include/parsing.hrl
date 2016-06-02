-ifndef(__parsing_hrl).
-define(__parsing_hrl, __parsing_hrl).
-include("../include/lex.hrl").

-define(addition, addition).
-define(factor, factor).
-define(op_sub, op_sub).
-define(op_div, op_div).

-record(expression, { type :: ?addition | ?factor,
		      op :: ?op_add | ?op_sub | ?op_mul | ?op_div,
		      no :: integer(),
		      left :: #expression{} | int1() | float1(),
		      right :: #expression{} | int1() | float1()
		    }).

-record(expression_info, { expression :: #expression{},
			   size :: integer() }).

-endif.
