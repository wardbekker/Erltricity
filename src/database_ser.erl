%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% singleton gen_server for inserting and fetching records to a MYSQL db.
%% @end
-module(database_ser).
-behaviour(gen_server).
-include("mindwave.hrl").

-define(SERVER, ?MODULE).
-define(POOL_ID, 1).

-record(state, {}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/1, insert/1, fetch/1, fetch/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

insert(Record) ->
    Table = erlang:element(1, Record),
    Fields = records:'#info-'(Table),
    Values = lists:map(
               fun(I) -> erlang:element(I, Record) end,
               lists:seq(2, erlang:length(Fields) + 1)
              ),
    Query = replace_param(
              <<"INSERT INTO ?P (?P) VALUES (?)">>,
              [Table, Fields, Values]
             ),
    {updated, _} = mysql:fetch(?POOL_ID, Query).

fetch(Query) ->
    fetch(Query, []).

fetch(Query, Params) ->
    Query1 = replace_param(Query,Params),
    { data, {mysql_result, _Headers, Values, _ , _ , _ } } =
        mysql:fetch(?POOL_ID, Query1),
    Values.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Config) ->
    Host = proplists:get_value(mysql_host, Config, "localhost"),
    User = proplists:get_value(mysql_user, Config, "root"),
    Password = proplists:get_value(mysql_password, Config, ""),
    Database = proplists:get_value(mysql_database, Config, "mindwave"),

    % open first connection
    mysql:start_link(
      ?POOL_ID, Host, User, Password, Database
     ),
    % open second connection 
    mysql:connect(
      ?POOL_ID, Host, undefined, 
      User, Password, Database, true
     ),
    {ok, #state{}}.

handle_call(stop, _From, State) ->
    Reason = 'stop requested',
    {stop, Reason, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% private Function Definitions
%% ------------------------------------------------------------------

-spec replace_param(binary(),[any()]) -> binary().
replace_param(<<"?P", Tail/binary>>, [ Param | NewParams ]) ->
    SeperatedParam = seperator(Param),
    ReplacedTail = replace_param(Tail, NewParams),
    erlang:list_to_binary([SeperatedParam, ReplacedTail]);
replace_param(<<"?", Tail/binary>>, [ Param | NewParams ]) ->
    EncodedParam = encode(Param),
    ReplacedTail = replace_param(Tail, NewParams),
    erlang:list_to_binary([EncodedParam, ReplacedTail]);
replace_param(<<Head:8, Tail/binary>>, Params) ->
    ReplacedTail = replace_param(Tail, Params),
    erlang:list_to_binary([Head, ReplacedTail]);
replace_param(_, []) ->
    <<>>.

-spec encode(any()) -> any().
encode(Param) when erlang:is_list(Param) ->
    case io_lib:printable_list(Param) of
        true ->
            mysql:encode(Param);
        false ->
            string:join(
              lists:map(fun(P) -> encode(printable(P)) end, Param), ", "
             )
    end;
encode(Param) ->
    mysql:encode(Param).

-spec seperator(any()) -> iolist().
seperator(Param) when erlang:is_list(Param) ->
    case io_lib:printable_list(Param) of
        true ->
            Param;
        false ->
            string:join(lists:map(fun(P) -> printable(P) end, Param), ", ")
    end;
seperator(Param) ->
     printable(Param).

-spec printable(any()) -> iolist().
printable(Param) ->
    lists:flatten(io_lib:format("~p",[Param])).
