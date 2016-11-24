%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <kflarsen@diku.dk>
%%% @author Oleksandr Shturmov <oleks@oleks.info>
%%% @copyright 2011–2016, Ken Friis Larsen
%%% @doc
%%%
%%% Functions for parsing the bag-of-words datasets from the
%%% musiXmatch dataset, the official lyrics collection for the Million
%%% Song Dataset, available at
%%% [http://labrosa.ee.columbia.edu/millionsong/musixmatch] (MXM).
%%%
%%% @end
%%% Last updated: Oct 2015 by Ken Friis Larsen <kflarsen@diku.dk>
%%% Last updated: Oct 2016 by Oleksandr Shturmov <oleks@oleks.info>
%%%   * Use Erlang -spec's instead of EDoc \@spec's.
%%%   * Added a -spec and EDoc for do_all/1.
%%%-------------------------------------------------------------------
-module(read_mxm).

%% API
-export([from_file/1, parse_track/1, do_all/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Read in and parse a bag-of-words dataset from the MXM project.
%%
%% Returns a pair where the first component is a list of words, and
%% the second component is a list tuples as returned by `parse_track'
%% below.
%%
%% @end
%%--------------------------------------------------------------------

-spec do_all(string()) ->
    { [string()], { binary(), binary(), [{ integer(), integer() }] } }.

do_all(FileName) ->
    {Words, Tracks} = from_file(FileName),
    {Words, [ parse_track(T) || T <- Tracks]}.

%%--------------------------------------------------------------------
%% @doc Read in a bag-of-words dataset from the MXM project.
%%
%% Returns a pair where the first component is a list of words, and
%% the second component is a list of binaries, one for each track. Use
%% `parse_track' to parse a binary track.
%%
%% @end
%%--------------------------------------------------------------------

-spec from_file(string()) -> { [string()], [binary()] }.

from_file(FileName) ->
    BLines = read_lines(FileName),
    [WLine | Tracks] = lists:filter(fun (<<$#, _/binary>>) -> false;
                                        (_)                -> true end,
                                    BLines),
    Words = parse_words_line(WLine),
    {Words,Tracks}.

read_lines(Filename) ->
    {ok, File} = file:open(Filename,
      [read, raw, binary, {read_ahead, 1024*1024}]),
    Result = process([], File),
    file:close(File),
    Result.

process(State, File) ->
    case file:read_line(File) of
        {ok, Data} ->
            NewState = [binary:part(Data, 0, byte_size(Data)-1)|State],
            process(NewState, File);
        eof        -> lists:reverse(State)
    end.


%%--------------------------------------------------------------------
%% @doc Parse a track from a binary on the form:
%%    `track_id, mxm_track_id, <word idx>:<cnt>, <word idx>:<cnt>, ...'
%%
%% Return a tuple where the first component is track_id, the second is
%% mxm_track_id, and the third is a list of pairs: word index and
%% count. Remember that word index starts at 1 (not zero).  See comment in
%% the start of the bag-of-words dataset for more detailed information.
%%
%% @end
%%--------------------------------------------------------------------

-spec parse_track(binary()) ->
    { binary(), binary(), [{ integer(), integer() }] }.

parse_track(Track) ->
    [TrackId, Rest1] = binary:split(Track, <<$,>>),
    [MxmId, Rest2] = binary:split(Rest1, <<$,>>),
    {TrackId,MxmId, parse_counts(Rest2, [])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_words_line(<<$%,WLine/binary>>) ->
    lists:map(fun binary_to_list/1, binary:split(WLine, <<$,>>, [global,trim])).

parse_counts(<<>>, Acc) ->
    lists:reverse(Acc);
parse_counts(<<$\n>>, Acc) ->
    lists:reverse(Acc);
parse_counts(<<$,, Rest/binary>>, Acc) ->
    parse_counts(Rest, Acc);
parse_counts(CBin, Acc) ->
    {Widx, Cnt, Rest} = parse_count(CBin),
    parse_counts(Rest, [{Widx, Cnt} | Acc]).

parse_count(CBin) ->
    {Widx, <<$:, Rest/binary>>} = parse_integer(CBin, 0),
    {Cnt, Rest2} = parse_integer(Rest, 0),
    {Widx, Cnt, Rest2}.

parse_integer(<<$0, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 0);
parse_integer(<<$1, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 1);
parse_integer(<<$2, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 2);
parse_integer(<<$3, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 3);
parse_integer(<<$4, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 4);
parse_integer(<<$5, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 5);
parse_integer(<<$6, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 6);
parse_integer(<<$7, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 7);
parse_integer(<<$8, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 8);
parse_integer(<<$9, Rest/binary>>, Acc) -> parse_integer(Rest, Acc * 10 + 9);
parse_integer(Rest, Acc) -> {Acc, Rest}.
