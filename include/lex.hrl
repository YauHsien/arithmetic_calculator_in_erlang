-ifndef(__lex_hrl).
-define(__lex_hrl, __lex_hrl).


-record(term, { type :: atom(),
		loc :: integer(),
		value :: any() }).

-endif.
