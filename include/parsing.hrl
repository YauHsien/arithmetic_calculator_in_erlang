-ifndef(__parsing_hrl).
-define(__parsing_hrl, __parsing_hrl).
-include("../include/lex.hrl").

-type int1() :: #term{ type :: ?numeral }.
-type float1() :: #term{ type :: ?float }.
-type op_fac() :: #term{ type :: ?op_mul }.
-type op_add() :: #term{ type :: ?op_add }.

-record(expression, { full = false :: boolean(),
		      op :: op_fac() | op_add(),
		      left :: #expression{} | int1() | float1(),
		      right :: #expression{} | int1() | float1()
		    }).

-type waiting_tree() :: [ #term{ type :: (?numeral|?float) } |
			  #expression{}
			].

-endif.
