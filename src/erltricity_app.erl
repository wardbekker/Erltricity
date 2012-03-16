%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Mindwave OTP app
%% @end
-module(erltricity_app).
-behaviour(application).

%% ------------------------------------------------------------------
%% application Function Exports
%% ------------------------------------------------------------------

-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% application Function Definitions
%% ------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    application:start(crypto),
    application:start(inets),
    application:start(mochiweb),
    application:start(webmachine),
    erltricity_sup:start_link().

stop(_State) ->
    ok.
