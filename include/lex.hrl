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
-define(lparen, lparen).
-define(rparen, rparen).
-define(not_accepted, not_accepted).
-define(head, head).
-define(non_head, non_head).

-record(term, { type :: ?backspace
		      | ?float
		      | ?numeral
		      | ?point
		      | ?digit
		      | ?op_add
		      | ?op_mul
		      | ?sep
		      | ?empty
		      | ?lparen
		      | ?rparen
		      | ?not_accepted,
		loc :: integer(),
		value :: any() }).

-type term1() :: ?sep | #term{}.
-type terms() :: [#term{}].
-type terms1() :: [term1()].

-endif.
