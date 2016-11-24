-module(gen_command).

-export([start/2, invoke/3, avast/2, ahoy/2, furl/1]).

start(Mod, Args) ->
    try Mod:initialise(Args) of
        {ok, State, Limit} ->
            ServerRef = spawn(fun() -> loop(Mod, [], [], [], State, Limit) end),
            {ok, ServerRef};
        Other -> 
            {error, Other}
    catch   
        Other -> 
            {error, Other}
    end.
    

invoke(ServerRef, Args, From) ->
    request_reply(ServerRef, {invoke, Args, From}).

avast(ServerRef, CID) ->
    async(ServerRef, {avast, CID}).

ahoy(ServerRef, CID) ->
    request_reply(ServerRef, {ahoy, CID}).

furl(ServerRef) ->
    request_reply(ServerRef, furl).
 



loop(Module, Running, Waiting, Completed, State, Limit) ->
    receive
        {Pid, {invoke, Args, From}} ->
            Me = self(),
            CID = spawn(fun() -> invoker(Me, Module, Args, State, From) end),
            reply(Pid, {Me, CID}),
            receive
                % Vi venter på, at invokeren er klar før vi går videre
                {CID, alive} ->
                    % Hvis der stadig er plads til en ekstra aktiv så kører vi den næste.
                    % Hvis ikke så sætter vi den i vente-kø.
                    case length(Running) < Limit of
                        true ->
                            NewRunning = Running ++ [CID],
                            startInvoker(CID),
                            loop(Module, NewRunning, Waiting, Completed, State, Limit);
                        false ->
                            NewWaiting = Waiting ++ [CID],
                            loop(Module, Running, NewWaiting, Completed, State, Limit)
                    end
            end;
        
        {_, {avast, CID}} ->
            async(CID, {self(), avast}),
            NewRunning = lists:delete(CID, Running),
            NewWaiting = lists:delete(CID, Waiting),
            NewCompleted = Completed ++ [CID],
            loop(Module, NewRunning, NewWaiting, NewCompleted, State, Limit);
            
        {Pid, {ahoy, CID}} ->
            case lists:any(fun(E) -> E == CID end, Running) of
                true ->
                    reply(Pid, running);
                false -> 
                    case lists:any(fun(E) -> E == CID end, Waiting) of
                        true -> 
                            reply(Pid, queued);
                        false ->
                            case lists:any(fun(E) -> E == CID end, Completed) of
                                true ->
                                    reply(Pid, completed);
                                false ->
                                    reply(Pid, {error, nomatch})
                            end
                    end
            end,
            loop(Module, Running, Waiting, Completed, State, Limit);
        
        {CID, {success, From, Reply}} ->
            reply(From, {success, Reply}),
            NewCompleted = Completed ++ [CID],
            NewRunning = lists:delete(CID, Running),
            % Vi ser om der er nogen som venter
            case length(Waiting) > 0 of
                true ->
                    Next = lists:nth(1, Waiting),
                    NewWaiting = lists:delete(Next, Waiting),
                    startInvoker(Next),
                    NewNewRunning = [Next] ++ NewRunning,
                    loop(Module, NewNewRunning, NewWaiting, NewCompleted, State, Limit);
                false ->
                    loop(Module, NewRunning, Waiting, NewCompleted, State, Limit)
            end;
        
        {CID, {failure, From, Reply}} ->
            reply(From, {failure, Reply}),
            NewCompleted = Completed ++ [CID],
            NewRunning = lists:delete(CID, Running),
            case length(Waiting) > 0 of
                true ->
                    Next = lists:nth(1, Waiting),
                    NewWaiting = lists:delete(Next, Waiting),
                    startInvoker(Next),
                    NewNewRunning = [Next] ++ NewRunning,
                    loop(Module, NewNewRunning, NewWaiting, NewCompleted, State, Limit);
                false ->
                    loop(Module, NewRunning, Waiting, NewCompleted, State, Limit)
            end;
        {Pid, furl} ->
            lists:foreach(fun(CID) ->
                          request_reply(CID, avast)
                          end,
                          Waiting),
            lists:foreach(fun(CID) ->
                            async(CID, {self(), avast})
                          end,
                          Running),
            Module:handle_furl(State),
            reply(Pid, ok);
        {Pid, Other} ->
            reply(Pid, {unknown_request, Other}),
            loop(Module, Running, Waiting, Completed, State, Limit);
        _ ->
            loop(Module, Running, Waiting, Completed, State, Limit)
    end.


invoker(Parent, Module, Args, State, From) ->
    Parent ! {self(), alive},
    receive
        {Parent, go} ->
            try Module:handle_invocation(Args, State) of
                {reply, Reply} ->
                    reply(Parent, {success, From, Reply});
                blimey ->
                    Module:carren(State, Args),
                    reply(Parent, {failure, From, aborted})
            catch
                Error ->
                    reply(Parent, {failure, From, Error})
            end;
        {Parent, avast} ->
            reply(Parent, ok),
            Module:carren(State, Args)
    end.    
            
    
    
%%% Synchronous communication 
reply(Pid, Reply) ->
    Pid ! {self(), Reply}.

async(Pid, Msg) ->
    Pid ! Msg.
    
request_reply(ServerRef, Msg) ->
    ServerRef ! {self(), Msg},
    receive {ServerRef, Reply} ->
        Reply
    end.
    
startInvoker(CID) ->
    Me = self(),
    async(CID, {Me, go}).