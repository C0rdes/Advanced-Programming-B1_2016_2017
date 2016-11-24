-module(erc).
-export([start/0, connect/2, chat/2, history/1, filter/3, plunk/2, censor/2]).

%%% API
start() -> {ok, spawn(fun () -> loop([], [], []) end)}.

%% connect(Server, Nick)
connect(Server, Nick) ->
	request_reply(Server, {connect, Nick}).
	
%% chat/2
chat(Server, Cont) ->
	nonblocking(Server, {chat, Cont}).
	
%% history/1
history(Server) ->
	request_reply(Server, history).
	
%% filter/3
filter(Server, Method, P) ->
	request_reply(Server, {filter, Method, P}).
	
%% plunk/2
plunk(Server, Nick) ->
	filter(Server, compose, 
		fun({Name, _}) -> 
			case Name of 
				Nick -> false; 
				_ -> true 
			end 
		end). 
	
%% censor/2	
censor(Server, Words) ->
	filter(Server, compose,
		fun({_, Msg}) -> 
			% assume that messages only contain the special characters ,.!?
			Tokens = string:tokens(Msg, " .,!?"), 
			case lists:any(fun(Token) -> lists:member(Token, Words) end, Tokens) of
				true ->
					false;
				_ ->
					true
			end
		end).

%%% Synchronous communication
	
request_reply(Server, Request) ->
    Server ! {self(), Request},
    receive
	{Server, Response} ->
	    Response
    end.
	
reply(From, Msg) ->
	From ! {self(), Msg}.

%%% Asynchronous communication
	
nonblocking(Server, Request) ->
	Server ! {self(), Request}.



%%% Internal implementation
	
%% loop should maintain a list of users and a list of the previous 42 messages

loop(Users, ChatHistory, Filters) ->
	receive
		{From, {connect, Nick}} ->
			% We reserve the nick admin for use by the server and an admin.
			case Nick == admin of
				false ->
					case lists:any(fun({N, _}) -> N == Nick end, Users) of
						true -> 
							reply(From, {error, Nick, is_taken}),
							loop(Users, ChatHistory, Filters);
						false -> 
							% Assuming that the same process can try connecting multiple times
							case lists:any(fun({_, Pid}) -> Pid == From end, Users) of
								true -> 
									reply(From, {error, From, already_connected}),
									loop(Users, ChatHistory, Filters);
								false ->
									reply(From, {ok, self()}),
									% We maintain a list of both the user nicks and process ID's.
									loop([{Nick, From}] ++ Users, ChatHistory, Filters)
							end
					end;
				true ->
					reply(From, {error, Nick, is_protected}),
					loop(Users, ChatHistory, Filters)
			end;
		{From, {chat, Cont}} ->
			case lists:keyfind(From, 2, Users) of
				{Nick, Pid} -> 
					case lists:flatlength(ChatHistory) == 42 of
						true ->
							NewChatHistory = lists:droplast(ChatHistory),
							BroadcastList = lists:delete({Nick, Pid}, Users),
							broadcastMsg({Nick, Cont}, BroadcastList, Filters),
							loop(Users, [{Nick, Cont}] ++ NewChatHistory, Filters);
						false ->
							BroadcastList = lists:delete({Nick, Pid}, Users),
							broadcastMsg({Nick, Cont}, BroadcastList, Filters),
							loop(Users, [{Nick, Cont}] ++ ChatHistory, Filters)
					end;
				false ->
					loop(Users, ChatHistory, Filters)
			end;
		{From, history} ->
			reply(From, {ChatHistory, self()}),
			loop(Users, ChatHistory, Filters);
		{From, {filter, replace, P}} ->
			% We assume that noone sends empty messages, and we reserve the nickname admin.
			% This serves two purposes -
			% 	1) We ensure that the predicate can actually be called and doesn't throw an exception with valid input
			%	2) We ensure that admin is not blocked. However, this would require and admin account be created in a later version.
			try P({admin, ""}) of
				true ->
					FiltersRemoved = lists:filter(fun({Pid, _}) -> Pid /= From end, Filters),
					reply(From, {ok, self()}),
					loop(Users, ChatHistory, [{From, P}] ++ FiltersRemoved);
				false ->
					reply(From, {error, P, cannot_block_admin}),
					loop(Users, ChatHistory, Filters);
				_ ->
					reply(From, {error, P, wrong_predicate_format}),
					loop(Users, ChatHistory, Filters)
			catch
				error ->
					reply(From, {error, P, wrong_predicate_format}),
					loop(Users, ChatHistory, Filters)
			end;	
		{From, {filter, compose, P}} ->
			% We assume that noone sends empty messages, and we reserve the nickname admin.
			% This serves two purposes -
			% 	1) We ensure that the predicate can actually be called and doesn't throw an exception with valid input
			%	2) We ensure that admin is not blocked. However, this would require and admin account be created in a later version.
			try P({admin, ""}) of
				true ->
					reply(From, {ok, self()}),
					loop(Users, ChatHistory, [{From, P}] ++ Filters);
				false ->
					reply(From, {error, P, cannot_block_admin}),
					loop(Users, ChatHistory, Filters);
				_ ->
					reply(From, {error, P, wrong_predicate_format}),
					loop(Users, ChatHistory, Filters)
			catch
				error ->
					reply(From, {error, P, not_a_predicate}),
					loop(Users, ChatHistory, Filters)
			end;
		{From, Other} ->
			reply(From, {error, Other, unknown_request}),
			loop(Users, ChatHistory, Filters)
	end.

% Used to send a message to all 
broadcastMsg(Msg, Users, Filters) ->
	lists:foreach(fun({_, Pid}) -> sendMsgToUser(Msg, Pid, Filters) end, Users).

	
sendMsgToUser(Msg, UserPid, Filters) ->
	{_, Predicates} = lists:unzip(lists:filter(fun({Pid, _}) -> Pid == UserPid end, Filters)),
	case lists:all(fun(Predicate) -> Predicate(Msg) end, Predicates) of
		true ->
			UserPid ! {Msg, self()};
		false ->
			error
	end.