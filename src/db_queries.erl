%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Database queries
%% @end
-module(db_queries).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [eSenses/2, eSense/3, poorSignal/2,
    blink/2, maxESenseTimestamp/0]
  ).

%% ------------------------------------------------------------------
%% Function Definitions
%% ------------------------------------------------------------------

maxESenseTimestamp() ->
    Query = <<"SELECT MAX(`timestamp`) FROM esense">>,
    [[TimeStamp]] = database_ser:fetch(Query),
    TimeStamp.

eSenses(StartTimestamp, EndTimestamp) ->
    database_ser:fetch(
      <<"SELECT e.`timestamp`, GROUP_CONCAT(e.`name`) as `name`,
GROUP_CONCAT(e.`strength`) as `strength` FROM esense e
WHERE e.`timestamp` BETWEEN ? AND ? AND e.`timestamp` NOT IN
(SELECT timestamp FROM poorsignal WHERE level > 10)
GROUP BY e.`timestamp` ORDER BY e.`timestamp`">>,
      [StartTimestamp, EndTimestamp]
     ).

eSense(Name, StartTimestamp, EndTimestamp) ->
    database_ser:fetch(
      <<"SELECT e.`timestamp`, GROUP_CONCAT(e.`name`) as `name`,
GROUP_CONCAT(e.`strength`) as `strength` FROM esense e
WHERE e.`name` = ? AND e.`timestamp` BETWEEN ? AND ? AND e.`timestamp` NOT IN
(SELECT timestamp FROM poorsignal WHERE level > 10)
GROUP BY e.`timestamp` ORDER BY e.`timestamp`">>,
      [Name, StartTimestamp, EndTimestamp]
     ).

poorSignal(StartTimestamp, EndTimestamp) ->
    Query = <<"SELECT `timestamp`, `level` FROM `poorsignal`",
              "WHERE `timestamp` BETWEEN ? AND ?">>,
    database_ser:fetch(Query, [StartTimestamp, EndTimestamp]).

blink(StartTimestamp, EndTimestamp)->
    Query = <<"SELECT `timestamp`, `strength` FROM `blink` ",
              "WHERE `timestamp` BETWEEN ? AND ?">>,
    database_ser:fetch(Query, [StartTimestamp, EndTimestamp]).

event(StartTimestamp, EndTimestamp)->
    Query = <<"SELECT `timestamp`, `name` FROM `event`",
              "WHERE `timestamp` BETWEEN ? AND ?">>,
    database_ser:fetch(Query, [StartTimestamp, EndTimestamp]).
