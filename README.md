# arithmetic_calculator_in_erlang
An arithmetic calculator in Erlang / OTP.

### Usage

To tokenize lexical symbols, send `{new_string, String}` to `formula_lex1` by using `gen_server:cast/2`:

> 1> {ok, Recv} = formula_lex2:start_link(self()), {ok, LexSrv} = formula_lex1:start_link(Recv).

> {ok,<0.34.0>}

> 2> gen_server:cast(LexSrv, {new_string, "hello,world"}).

> {add_term,{term,not_accepted,1,[104,101,108,108,111,44,119,111,114,108,100]}} received

> ok

> 3> gen_server:cast(LexSrv, {new_string, "1+(2-3)*4"}).

> drop_term received

> drop_term received

> ok

> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> drop_term received

> 4> {add_term,{term,numeral,1,[49]}} received

> 4> {add_term,{term,op_add,2,[43]}} received

> 4> {add_term,{term,lparan,3,undefined}} received

> 4> {add_term,{term,numeral,4,[50]}} received

> 4> {add_term,{term,op_add,5,[45]}} received

> 4> {add_term,{term,numeral,6,[51]}} received

> 4> {add_term,{term,rparan,7,undefined}} received

> 4> {add_term,{term,op_mul,8,[42]}} received

> 4> {add_term,{term,numeral,9,[52]}} received
> 4>

If every time you type some keys in the user interface, a `{new_string, "some thing you typed"}` will be casted to `formula_lex1`. The tokenizer server will keep track of all that you typed. Difference between two version of `new_string`s are retrieved, then `drop_term` and `{add_term, Term}` are produced.

### Syntax

Symbol acceptable: `0-9`, `.`, `+`, `-`, `*`, `/`, `(`, `)`, `<space>`.

Example acceptable: `1+ (2 -    3.14) * 4`

Example not acceptable: `hello,world`
