-module(callback_module).

-export([start/0, initialise/1, handle_invocation/2]).

-import(gen_command, [start/2, invoke/3, avast/2, ahoy/2, furl/1]).

start() ->
	gen_command:start(?MODULE, 5).

initialise(Args) ->
	{ok, Args, 5}.
	
handle_invocation(Args, State) ->
	Result = Args + State,
	{reply, Result}.
	
	
carrent(_State, _Args) ->
	undefined.
	