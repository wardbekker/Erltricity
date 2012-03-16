%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% provides exprecs utility functions for exported records.
%% @end
-module(records).
-compile({parse_transform, exprecs}).
-include("erltricity.hrl").
-export_records([blink, poorsignal, esense, event]).

