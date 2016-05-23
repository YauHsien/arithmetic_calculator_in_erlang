-ifndef(__lex_hrl).
-define(__lex_hrl, __lex_hrl).

-define(backspace, backspace).
-define(float, float).
-define(numeral, numeral).
-define(point, point).
-define(digit, digit).
-define(op_add, op_add).
-define(op_mul, op_mul).
-define(sep, sep).
-define(empty, empty).
-define(lparan, lparan).
-define(rparan, rparan).
-define(not_accepted, not_accepted).

-record(term, { type :: ?backspace
		      | ?float
		      | ?numeral
		      | ?point
		      | ?digit
		      | ?op_add
		      | ?op_mul
		      | ?sep
		      | ?empty
		      | ?lparan
		      | ?rparan
		      | ?not_accepted,
		loc :: integer(),
		value :: any() }).

-type term1() :: ?sep | #term{}.
-type terms() :: [#term{}].
-type terms1() :: [term1()].

-endif.
