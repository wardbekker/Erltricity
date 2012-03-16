%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Definition of records that are shared between modules
%% @end
-record(blink, { timestamp, strength }).
-record(poorsignal, { timestamp, level }).
-record(esense, { timestamp, name, strength }).
-record(event, { timestamp, name }).
