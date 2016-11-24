-module(mr).
-compile(mr).
-behaviour(gen_server).

-export([simple_test/0]).
-export([start/0, job/6, stop/1]).
-export([init/1, handle_call/3, terminate/2, code_change/3, handle_cast/2, handle_info/2]).


%%% API

%% start
start() -> 
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% job
job(Pid, NWorkers, MapFun, RedFun, Initial, Data) -> 
	gen_server:call(?MODULE, {job, Pid, NWorkers, MapFun, RedFun, Initial, Data}).

%% Stops master and and the reducerhandler process
stop(Pid) -> 
	gen_server:stop(Pid).

	
%%% Callback Functions

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	Pid = self(),
	Reducerhandler = spawn(fun() -> reducerhandler(Pid, [], [], init) end),
	{ok, Reducerhandler}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------	

handle_call({job, Pid, NWorkers, MapFun, {RedFun, Mode}, Initial, Data}, _From, Reducerhandler) ->
	%% We get the length of the data list.
	%% The idea is that the reducerhandler uses this information to determine when
	%% all the mapped data has been passed through the reducerhandler
	Remaining = length(Data), 
	case Mode of
		single ->
			NMappers = NWorkers - 1,
			NReducers = 1;
		multi ->
			NMappers = round(NWorkers * (1/2)), % rougly 1/2 of the workers are mappers, rest are reducers
			NReducers = NWorkers - NMappers
	end,
	Mappers = [spawn(fun() -> mapperloop(Pid, Reducerhandler, MapFun) end) || _ <- lists:seq(1, NMappers)],
	% --------
	% We request a reply because we want to make sure that the other process is done setting
	% up it's reducers.
	% --------
	request_reply(Reducerhandler, {init, NReducers, RedFun, Remaining, Initial}),
	% The data is first sent to the mappers. Then from the mappers to the reducerhandler (see code below)
	send_data(Mappers, Data),
	receive
		{Reducerhandler, {result, Result}} ->
			lists:foreach(fun(Mapper) -> request_reply(Mapper, stop) end, Mappers),
			{reply, {ok, Result}, Reducerhandler}
	end.
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%
%% Not used but has to be there
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}. 

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%
%% Not used but has to be there
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, Reducerhandler) ->
	% We ask the reducerhandler to clean itself up
	request_reply(Reducerhandler, stop),
    ok.
	
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%
%% Not used but has to be there
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


	
%%% Synchronous communication

request_reply(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.
	
reply(From, Msg) ->
	From ! {self(), Msg}.

reply_result(Pid, Result) ->
	reply(Pid, {result, Result}).
	
%%% Asynchronous communication

async(Pid, Msg) ->
	Pid ! Msg.

async_data(Pid, Data) ->
	async(Pid, {data, Data}).

async_reduce(Pid, Result) ->
	async(Pid, {reduce, Result}).
	
%% Method for sending large amounts of data to different receivers one chunk at a time. 
%% In this case mappers
send_data(Mappers, Data) ->
	send_loop(Mappers, Mappers, Data).

%% The loop which implements the above send_data method
send_loop(Mappers, [Mapper|Rest], [D|Data]) ->
	async_data(Mapper, D),
	send_loop(Mappers, Rest, Data);
send_loop(Mappers, [], Data) -> 
	% If the end of the queue is reached we start the queue over
	send_loop(Mappers, Mappers, Data);
send_loop(_, _, []) ->
	ok.


%%% Mapper code	
	
%% The loop which handles the mapper function and forwards the mapped data to the reducerhandler
mapperloop(Pid, Reducerhandler, Fun) ->
	receive 
		{Pid, stop} ->
			reply(Pid, ok);
		{data, Data} ->
			Result = Fun(Data),
			% The mapped result is sent to the reducerhandler and the mapper is ready to accept a new message
			async_reduce(Reducerhandler, Result),
			mapperloop(Pid, Reducerhandler, Fun)
	end.

	
%%% Reducer code

%% Handler for multiple reducers
reducerhandler(Pid, Reducers, Reducerqueue, Remaining) ->
	if 
		%% probably bad code practice, but it works
		% We assume that Remaining is init or a nonnegative number.
		Remaining == init ->
			receive
				{Pid, {init, NReducers, RedFun, NewRemaining, Initial}} ->
					Me = self(),
					NewReducers = [spawn(fun() -> reducerloop(Me, RedFun, Initial) end ) 
									|| _ <- lists:seq(1, NReducers)],
					reply(Pid, ok),
					reducerhandler(Pid, NewReducers, NewReducers, NewRemaining);
				{Pid, stop} ->
					reply(Pid, ok)
			end;
		Remaining > 0 ->	
			receive
				{reduce, Result} ->
					NextReducer = hd(Reducerqueue),
					async_reduce(NextReducer, Result),
					case tl(Reducerqueue) of
						[] ->
							reducerhandler(Pid, Reducers, Reducers, Remaining - 1);
						_ ->
							reducerhandler(Pid, Reducers, tl(Reducerqueue), Remaining -1)
					end
			end;
		Remaining == 0 ->
			%% If remaining is 0 we start preparing for returning one value.
			%% We get the value of the first reducer.
			Acc = request_reply(hd(Reducers), return),
			%% If more reducers exists we pass the value from the previous reducers to each reducer
			%% to calculate the final value. See reduceresults code below
			Result = reduceresults(tl(Reducers), Acc),
			reply_result(Pid, Result),
			%% The reducerhandler is returned to the initial state.
			reducerhandler(Pid, [], [], init)
	end.
	
%% reducer implementation	
%% The function responsible for handling the reduction function
%% The reducerloop stores the accumulated value and returns it to the reducerhandler only on demand.
reducerloop(Reducerhandler, RedFun, Acc) ->
	receive
		{reduce, Result} ->
			%% The reducer is asked to reduce a result from a mapper.
			NewAcc = RedFun(Result, Acc),
			reducerloop(Reducerhandler, RedFun, NewAcc);
		{Reducerhandler, return} ->
			%% The reducer is asked to return its accumulated answer
			reply(Reducerhandler, Acc);
		{Reducerhandler, {syncreduce, Result}} ->
			%% syncreduce is used when the reducerhandler process wants a new value calculated and returned.
			%% Used by the reduceresults branch below.
			LastAcc = RedFun(Result, Acc),
			reply(Reducerhandler, LastAcc)
	end.
	
	
%% method for getting the results from the reducers

	
%% method for reducing the returned results into one result
reduceresults([], Acc) ->
	%% If there are no more reducers we just want final value.
	Acc;

reduceresults([Reducer|Rest], Acc) ->
	%% The first reducer in the queue is sent the accumulated value and is asked to 
	%% perform RedFun(ReducerAcc, Acc) and return the value.
	NewAcc = request_reply(Reducer, {syncreduce, Acc}),
	reduceresults(Rest, NewAcc).
	