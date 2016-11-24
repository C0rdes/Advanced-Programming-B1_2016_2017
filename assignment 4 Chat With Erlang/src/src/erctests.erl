-module(erctests).
-import(erc, [start/0, connect/2, chat/2, history/1, filter/3, plunk/2, censor/2]).

-export([startTest1/0, startTest2/0]).
-export([connectTest1/0, connectTest2/0, connectTest3/0, connectTest4/0, connectTest5/0]).
-export([chatTest1/0, chatTest2/0]).
-export([historyTest1/0, historyTest2/0]).
-export([filterTest1/0, filterTest2/0, filterTest3/0, filterTest4/0]).
-export([plunkTest1/0, plunkTest2/0]).
-export([censorTest/0]).

%%% Start test

% tests if we can start a ERC server
startTest1() ->
	{ok, Ref} = erc:start(),
	Ref.

% tests if we can start multiple ERC servers
startTest2() ->
	{ok, Ref1} = erc:start(),
	{ok, Ref2} = erc:start(),
	{ok, Ref3} = erc:start(),
	{Ref1, Ref2, Ref3}.

%%% Connect tests

% tests if we can connect to an ERC server
connectTest1() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user),
	Ref.

% tests if we can connect to a server with multiple different clients
connectTest2() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	Client1 = spawn(fun() -> 
						Ret = erc:connect(Ref, user1), 
						Me ! {self(), Ret}
					end),
	receive
		{Client1, Ret1} ->
			Ret1
	end,
	Client2 = spawn(fun() -> 
						Ret = erc:connect(Ref, user2), 
						Me ! {self(), Ret}
					end),
	receive
		{Client2, Ret2} ->
			Ret2
	end,
	Client3 = spawn(fun() -> 
						Ret = erc:connect(Ref, user3), 
						Me ! {self(), Ret}
					end),
	receive
		{Client3, Ret3} ->
			Ret3
	end,
	{Ret1, Ret2, Ret3}.
	
% tests if the server catches when a user tries to log on with the protected admin name
connectTest3() ->
	{ok, Ref} = erc:start(),
	{error, admin, is_protected} = erc:connect(Ref, admin).

% tests if the server catches when a username is occupied	
connectTest4() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	_ = spawn(fun() -> 
				erc:connect(Ref, user),
				Me ! done
			  end),
	receive
		done -> ok
	end,
	{error, user, is_taken} = erc:connect(Ref, user).

% tests if the server catches if a user logging in multiple times
connectTest5() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	_ = erc:connect(Ref, user),
	{error, Me, already_connected} = erc:connect(Ref, user2).

%%% Chat tests

% tests if a user receives a message sent from another user
chatTest1() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	Spawn = spawn(fun() -> 
					erc:connect(Ref, user2),
					Me ! done,
					receive
						Msg ->
							Me ! {self(), Msg}
					end
			      end),
	receive
		done ->
			ok
	end,
	erc:chat(Ref, "hello"),
	receive
		{Spawn, Msg} ->
			Msg
	end.

% Tests if multiple users receive a message sent from one user.	
chatTest2() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	spawn(fun() -> 
					erc:connect(Ref, user2),
					Me ! done,
					receive
						Msg ->
							Me ! Msg
					end
			      end),
	receive
		done ->
			ok
	end,
	spawn(fun() -> 
					erc:connect(Ref, user3),
					Me ! done,
					receive
						Msg ->
							Me ! Msg
					end
			      end),
	receive
		done ->
			ok
	end,
	erc:chat(Ref, "hello"),
	receive
		Msg1 ->
			Msg1
	end,
	receive
		Msg2 ->
			Msg2
	end,
	{Msg1, Msg2}.
	
%%% History tests

% Test which
historyTest1() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	erc:chat(Ref, "msg1"),
	erc:chat(Ref, "msg2"),
	spawn(fun () -> 
			erc:connect(Ref, user2),
			erc:chat(Ref, "msg3"),
			erc:chat(Ref, "msg4"),
			erc:chat(Ref, "msg5"),
			Me ! done
		  end),
	receive
		done ->
			ok
	end,
	erc:chat(Ref, "msg6"),
	erc:chat(Ref, "msg7"),
	{Ret, Ref} = erc:history(Ref),
	Ret.


%%% Filter tests

% tests if a filter works by filtering the messages from a user
filterTest1() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	{ok, Ref} = erc:filter(Ref, compose, fun({_, Msg}) -> 
											case Msg of
												"hello" -> 
													false;
												_ ->
													true
											end
										  end),
	spawn(fun () ->
			erc:connect(Ref, user2),
			erc:chat(Ref, "hello"),
			erc:chat(Ref, "hi")
		  end),
	receive
		{Msg, Ref} ->
			Msg
	end.
	
% tests if a filter can be replaced
filterTest2() ->
	Me = self(),
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	spawn(fun () ->
			erc:connect(Ref, user2),
			erc:chat(Ref, "hello"),
			erc:chat(Ref, "hi"),
			Me ! done
		  end),
	receive
		done ->
			ok
	end,
	receive
		_ ->
			receive 
			_ ->
				ok
			end
	end,
	{ok, Ref} = erc:filter(Ref, compose, fun({_, Msg}) -> 
											case Msg of
												"hello" -> 
													false;
												_ ->
													true
											end
										  end),
	{ok, Ref} = erc:filter(Ref, replace, fun({_, Msg}) -> 
											case Msg of
												"hi" -> 
													false;
												_ ->
													true
											end
										  end),
	spawn(fun () ->
			erc:connect(Ref, user3),
			erc:chat(Ref, "hello"),
			erc:chat(Ref, "hi")
		  end),
	receive
		{Msg, Ref} ->
			Msg
	end.
	
filterTest3() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user),
	{error, _, wrong_predicate_format} = erc:filter(Ref, replace, 
													fun({_, _}) -> kage end).
	
% tests whether the filter function	with the compose tag	
filterTest4() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	{error, _, wrong_predicate_format} = erc:filter(Ref, compose, 
													fun({_, _}) -> kage end).

%%% Plunk tests

plunkTest1() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	{ok, Ref} = erc:plunk(Ref, user2),
	Spawn = spawn(fun() ->
				erc:connect(Ref, user2),
				receive
					go ->
						erc:chat(Ref, "hello")
				end
			  end),
	Spawn ! go,		  
	receive 
		Msg ->
			Msg
	after 5000 ->
		io:format("plunkTest timed out", [])
	end.
	

plunkTest2() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	{error, _, cannot_block_admin} = erc:plunk(Ref, admin).

%%% Censor tests

censorTest() ->
	{ok, Ref} = erc:start(),
	{ok, Ref} = erc:connect(Ref, user1),
	{ok, Ref} = erc:censor(Ref, ["kage"]),
	Spawn = spawn(fun() ->
					erc:connect(Ref, user2),
					receive 
						go ->
							erc:chat(Ref, "kage")
					end
				  end),
	Spawn ! go,	 
	receive 
		Msg ->
			Msg
	after 5000 ->
		io:format("censorTest timed out", [])
	end.
