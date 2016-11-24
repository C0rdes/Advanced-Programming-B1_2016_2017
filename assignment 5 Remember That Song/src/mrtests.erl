-module(mrtests).
-import(mr, [start/0, job/5, stop/1]).

-export([simple_test/0]).

simple_test() ->
        {ok, MR}  = mr:start(),
        {ok, Sum} = mr:job(MR,
                           3,
                           fun(X) -> X end,
                           {fun(X, Acc) -> X + Acc end, multi},
                           0,
                           lists:seq(1,10)),
        {ok, Fac} = mr:job(MR,
                           4,
                           fun(X) -> X end,
                           {fun(X, Acc) -> X * Acc end, multi},
                           1,
                           lists:seq(1,20)),
        mr:stop(MR),
        {Sum, Fac}.

