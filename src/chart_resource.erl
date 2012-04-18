%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Webmachine resource for displaying the eSense data as line charts.
%% @end
-module(chart_resource).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/1, to_html/2, to_json/2, content_types_provided/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init([]) -> {ok, undefined}.

content_types_provided(ReqData, State) ->
    Types = [{"application/json", to_json},{"text/html", to_html}],
    {Types, ReqData, State}.


%% @doc Provides fresh datapoints for the specified chart type
to_json(ReqData, State) ->
    LastMaxTimestamp = erlang:list_to_integer(
                         wrq:get_qs_value("maxValue", ReqData)
                        ),
    StartTimestamp = LastMaxTimestamp + 1,
    EndTimestamp = db_queries:maxESenseTimestamp(),
    ChartDataType = wrq:get_qs_value("chartDataType", ReqData),
    Data = chartData(ChartDataType, StartTimestamp, EndTimestamp),
    Result = proplists:get_value("rows", Data),
    {mochijson2:encode(Result), ReqData, State}.

%% @doc Render the charts with existing datapoints
to_html(ReqData, State) ->
    EndTimestamp = db_queries:maxESenseTimestamp(),
    StartTimestamp = EndTimestamp - utils:timestamp({0,1000,0}),
    Charts = [
              "meditation", "theta", "highBeta",
              "highGamma", "attention", "lowBeta",
              "delta", "highAlpha", "lowGamma",
              "lowAlpha", "combined", "poorsignal", "blink"
             ],
    ChartsData = lists:map(
                   fun(C) -> chartData(C, StartTimestamp, EndTimestamp) end,
                   Charts),
    Data = [{charts, ChartsData} ],
    {ok, Content} = linechart_dtl:render(Data),
    {Content, ReqData, State}.

%% ------------------------------------------------------------------
%% private Function Definitions
%% ------------------------------------------------------------------

-spec chartData(iolist(), integer(), integer()) -> list().
chartData(Type, StartTimestamp, EndTimestamp) ->
    case Type of
        "blink" ->
            blinkData(StartTimestamp, EndTimestamp);
        "poorsignal" ->
            poorSignalData(StartTimestamp, EndTimestamp);
        "combined" ->
            eSenseData(StartTimestamp, EndTimestamp);
        ESense ->
            eSenseData(ESense, StartTimestamp, EndTimestamp)
    end.

-spec blinkData(integer(), integer()) -> list().
blinkData(StartTimestamp, EndTimestamp) ->
    DataRows = lists:map(
                 fun( R ) ->
                         {
                      erlang:integer_to_list(lists:nth(1, R)),
                      lists:nth(2, R)
                     }
                 end,
                 db_queries:blink(StartTimestamp, EndTimestamp)
                ),
    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1012"},
            {"height", "200"},
            {"title", "Blink"},
            {"chartDataType", "blink"},
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
                 db_queries:poorSignal(StartTimestamp, EndTimestamp)
                ),
    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1024"},
            {"height", "200"},
            {"title", "Poor Signal"},
            {"chartDataType", "poorsignal"},
            {"columns", [{ "number", "Timestamp"}, { "number", "level" }]},
            {"rows", DataRows }
           ],
    Data.

-spec eSenseData(integer(), integer()) -> list().
eSenseData(StartTimestamp, EndTimestamp) ->
    Res = db_queries:eSenses(StartTimestamp, EndTimestamp),
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
            {"chartDataType", "combined"},
            {"columns", DataColumns2},
            {"rows", DataRows }
           ],
    Data.

-spec eSenseData(iolist(), integer(), integer()) -> list().
eSenseData(Esense, StartTimestamp, EndTimestamp) ->
    Res = db_queries:eSense(Esense, StartTimestamp, EndTimestamp),
    case Res of
        [] ->
            DataColumns = [{"number", "Timestamp"}],
            DataRows = [];
        _ -> 
            DataCol = re:split(lists:nth(2,lists:nth(1, Res)), ","),
            DataColumns = [{"number", "Timestamp"}] ++ lists:map(fun(H) -> { "number", H } end, DataCol),
            DataRows = lists:map(
                         fun( R ) ->
                                 {
                              erlang:integer_to_list(lists:nth(1, R)),
                              erlang:list_to_integer(erlang:binary_to_list(lists:nth(3, R)))
                             }
                         end, Res)
    end,
    Data = [
            {"hAxis_minValue", StartTimestamp},
            {"hAxis_maxValue", EndTimestamp},
            {"width", "1024"},
            {"height", "200"},
            {"title", Esense},
            {"chartDataType", Esense},
            {"columns", DataColumns},
            {"rows", DataRows }
           ],
    Data.
