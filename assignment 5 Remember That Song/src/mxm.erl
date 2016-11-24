-module(mxm).
-import(mr, [start/0, job/5, stop/1]).
-import(mxm, [from_file/1, parse_track/1, do_all/1]).

-export([task1/0, task2/0, grep/1]).


%%% Task 1: Compute the total number of words, summed over all songs

task1() ->
	%% We first import the data from the data set via mxm
	{_, Tracks} = read_mxm:from_file("mxm_dataset_test.txt"),
	{ok, MR} = mr:start(),
	{ok, WordCountList} = mr:job(MR,
								5,
								%% Mapper function which extracts the word counts from the track info
								fun(Track) -> 
									{_, _, WordCounts} = read_mxm:parse_track(Track),
									WordCounts 
								end,
								%% Reducer function which appends the word counts to eachother to form a list
								{fun(WordCounts, Acc) -> WordCounts ++ Acc end, single},
								[],
								Tracks),
	{ok, SumOfWords} = mr:job(MR,
							 10,
							 fun({_Words, Count}) -> Count end,
							 {fun(Count, Acc) -> Count + Acc end, multi},
							 0,
							 WordCountList),
	mr:stop(MR),
	SumOfWords.
	
task2() ->
	{_, Tracks} = read_mxm:from_file("mxm_dataset_test.txt"),
	{ok, MR} = mr:start(),
	{ok, SongList} = mr:job(MR,
							5,
							fun(Track) -> 
								{_, _, WordCounts} = read_mxm:parse_track(Track),
								WordCounts
							end,
							{fun(WordCounts, Acc) -> [WordCounts] ++ Acc end, single},
							[],
							Tracks),
	{ok, {MeanUniqueWords, MeanTotalWords, _Index}} = mr:job(MR,
													3,
													fun(Song) -> 
														{length(Song), wordsinsong(Song, 0), 1} 
													end,
													{fun({UniqueWords, TotalWords, 1}, {MeanUnique, MeanTotal, Index}) ->
														NewIndex = Index + 1,
														NewMeanUnique = (MeanUnique * Index)/NewIndex + UniqueWords/NewIndex,
														NewMeanTotal = (MeanTotal * Index)/NewIndex + TotalWords/NewIndex,
														{NewMeanUnique, NewMeanTotal, NewIndex}
													end, single},
													{0,0,0},
													SongList),
	mr:stop(MR),
	{MeanUniqueWords, MeanTotalWords}.											

	
wordsinsong([], Sum) ->
	Sum;
	
wordsinsong([{_Key, Value}|Rest], Sum) ->
	NewSum = Sum + Value,
	wordsinsong(Rest, NewSum).
	
	
grep(Word) ->
	{Words, Tracks} = read_mxm:from_file("mxm_dataset_test.txt"),
	Index = get_index(Words, Word),
	{ok, MR} = mr:start(),
	{ok, SongList} = mr:job(MR,
							5,
							fun(Track) -> 
								{TID, _, WordCounts} = read_mxm:parse_track(Track),
								{TID, WordCounts}
							end,
							{fun(Song, Acc) -> [Song] ++ Acc end, single},
							[],
							Tracks),
	{ok, TIDList} = mr:job(MR,
							10,
							fun({TID, WordCounts}) ->
								case lists:keyfind(Index, 1, WordCounts) of
									{_Index, _} ->
										[TID];
									false ->
										[]
								end
							end,
							{fun(Msg, Acc) ->
								Msg ++ Acc
							end, single},
							[],
							SongList),
	mr:stop(MR),
	TIDList.



get_index(L, Word) ->
	get_index(L, Word, 1).
	
get_index([], _, _) -> not_found;
get_index([Word|_], Word, Index) ->
	Index;
get_index([_|Rest], Word, Index) ->
	get_index(Rest, Word, Index+1).

