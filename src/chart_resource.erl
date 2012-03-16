%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Webmachine resource for displaying the eSense data as line charts.
%% @end
-module(chart_resource).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/1, to_html/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    EndTimestamp = utils:maxESenseTimestamp(),
    StartTimestamp = EndTimestamp - utils:timestamp({0,1000,0}),
    Charts = [
              "\"meditation\"", "\"theta\"", "\"highBeta\"",
              "\"highGamma\"", "\"attention\"", "\"lowBeta\"",
              "\"delta\"", "\"highAlpha\"", "\"lowGamma\"",
              "\"lowAlpha\""
             ],
    ChartsData = lists:map(
                   fun(C) -> eSenseData(C, StartTimestamp, EndTimestamp) end,
                   Charts),
    Data = [{charts, [
                      eSenseData(StartTimestamp, EndTimestamp),
                      blinkData(StartTimestamp, EndTimestamp),
                      poorSignalData(StartTimestamp, EndTimestamp)
                     ]
             ++
                 ChartsData
            } ],
    {ok, Content} = linechart_dtl:render(Data),
    {Content, ReqData, State}.

%% ------------------------------------------------------------------
%% private Function Definitions
%% ------------------------------------------------------------------

-spec blinkData(integer(), integer()) -> list().
blinkData(StartTimestamp, EndTimestamp) ->
    DataRows = lists:map(
                 fun( R ) ->
                         {
                      erlang:integer_to_list(lists:nth(1, R)),
                      lists:nth(2, R)
                     }
                 end,
                 utils:blink(StartTimestamp, EndTimestamp)
                ),
    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1012"},
            {"height", "200"},
            {"title", "Blink"},
            {"columns", [{ "number", "Timestamp"}, { "number", "strength" }]},
            {"rows", DataRows }
           ],
    Data.

-spec poorSignalData(integer(), integer()) -> list().
poorSignalData(StartTimestamp, EndTimestamp) ->
    DataRows = lists:map(
                 fun( R ) ->
                         {
                      erlang:integer_to_list(lists:nth(1, R)),
                      lists:nth(2, R)
                     }
                 end,
                 utils:poorSignal(StartTimestamp, EndTimestamp)
                ),
    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1024"},
            {"height", "200"},
            {"title", "Poor Signal"},
            {"columns", [{ "number", "Timestamp"}, { "number", "level" }]},
            {"rows", DataRows }
           ],
    Data.

-spec eSenseData(integer(), integer()) -> list().
eSenseData(StartTimestamp, EndTimestamp) ->
    Res = utils:eSenses(StartTimestamp, EndTimestamp),
    DataColumns1 = re:split(lists:nth(2,lists:nth(1, Res)), ","),
    DataColumns2 = [
                    {"number", "Timestamp"}] ++
        lists:map(fun(H) -> { "number", H } end, DataColumns1),
    DataRows = lists:map(
                 fun( R ) ->
                         {
                      erlang:integer_to_list(lists:nth(1, R)),
                      lists:nth(3, R)
                     }
                 end, Res),

    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1024"},
            {"height", "200"},
            {"title", "Combined eSenses"},
            {"columns", DataColumns2},
            {"rows", DataRows }
           ],
    Data.

-spec eSenseData(iolist(), integer(), integer()) -> list().
eSenseData(Esense, StartTimestamp, EndTimestamp) ->
    Res = utils:eSense(Esense, StartTimestamp, EndTimestamp),
    DataColumns1 = re:split(lists:nth(2,lists:nth(1, Res)), ","),
    DataColumns2 = [{"number", "Timestamp"}] ++ lists:map(fun(H) -> { "number", H } end, DataColumns1),
    DataRows = lists:map(
                 fun( R ) ->
                         {
                      erlang:integer_to_list(lists:nth(1, R)),
                      erlang:list_to_integer(erlang:binary_to_list(lists:nth(3, R)))
                     }
                 end, Res),
    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1024"},
            {"height", "200"},
            {"title", Esense},
            {"columns", DataColumns2},
            {"rows", DataRows }
           ],
    Data.
