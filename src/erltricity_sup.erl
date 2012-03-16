%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% application supervisor
%% @end
-module(erltricity_sup).
-behaviour(supervisor).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, [Args]}, permanent, 5000, Type, [I]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    Children = [
                ?CHILD(database_ser, worker, read_config("database.config")),
                ?CHILD(connection_ser, worker, read_config("connector.config")),
                webmachine_child_spec()],
    {ok, { {one_for_one, 5, 10}, Children} }.

read_config(Filename) ->
    {ok, [Config]} = file:consult(filename:join(
                                  [filename:dirname(code:which(?MODULE)),
                                   "..", "config", Filename])),
    Config.

webmachine_child_spec() ->
    {webmachine_mochiweb,
     {
       webmachine_mochiweb, start, [read_config("webmachine.config")]
     },
     permanent, 5000, worker, dynamic
    }.
