-ifndef(__test_hrl).
-define(__test_hrl, __test_hrl).
-include("../include/lex.hrl").

%% 2+(4-1)*3
-define(tt1, #term{ type= ?numeral, loc= 1, value= "2" }).
-define(tt2, #term{ type= ?op_add, loc= 2, value= "+" }).
-define(tt3, #term{ type= ?lparen, loc= 3, value= "(" }).
-define(tt4, #term{ type= ?numeral, loc= 4, value= "4" }).
-define(tt5, #term{ type= ?op_add, loc= 5, value= "-" }).
-define(tt6, #term{ type= ?numeral, loc= 6, value= "1" }).
-define(tt7, #term{ type= ?rparen, loc= 7, value= ")" }).
-define(tt8, #term{ type= ?op_mul, loc= 8, value= "*" }).
-define(tt9, #term{ type= ?numeral, loc= 9, value= "3" }).

-endif.
