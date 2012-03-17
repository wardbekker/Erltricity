%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Utility/misc functions
%% @end
-module(utils).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([timestamp/1, time_tuple/1]).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------

timestamp({Mega, Secs, Micro}) ->
    Mega*1000*1000*1000*1000 + Secs * 1000 * 1000 + Micro.

time_tuple(Timestamp) ->
    {
      Timestamp div 1000000000000,
      Timestamp div 1000000 rem 1000000,
      Timestamp rem 1000000
    }.
