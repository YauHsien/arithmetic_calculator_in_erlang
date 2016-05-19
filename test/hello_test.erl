-module(hello_test).
-include_lib("eunit/include/eunit.hrl").


hello_test() ->
    %% ?assertEqual(true, false).
    ?assertEqual(true, true),
    ?assertEqual(false, false).

