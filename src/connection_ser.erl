%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% gen_server TCP client which connects to ThinkGear Connector,
%% parses the incomping packets and persists them into a database
%% @end
-module(connection_ser).
-behaviour(gen_server).
-include("erltricity.hrl").

-define(SERVER, ?MODULE).
-define(DEF_CONNECTOR_HOSTNAME, "127.0.0.1").
-define(DEF_CONNECTOR_PORT, 13854).
-define(
   CONNECTOR_INIT_MSG,
   "{\"enableRawOutput\": false, \"format\": \"Json\"}\n"
).
-define(SYNC_BYTE, 170).
-define(EX_CODE_BYTE, 85).

-record(state, { socket }).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link([]) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {'ok',#state{socket::port()}}.
init(Config) ->
    Hostname = proplists:get_value(
                 connector_hostname, Config, DEF_CONNECTOR_HOSTNAME
                ),
    Port = proplists:get_value(
             connector_port, Config, DEF_CONNECTOR_PORT
            ),
    {ok, Socket} = gen_tcp:connect(
                     Hostname, Port, [binary, {packet, 0}]
                    ),
    gen_tcp:send(Socket, ?CONNECTOR_INIT_MSG),
    State = #state{ socket = Socket},
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Port, Data}, State) ->
    %% Data can be non-json when init_msg
    %% is not processed by the connector, exiting the server
    %% Data can consists of multiple Packets joined with a Return
    lists:map(
      fun(Packet) ->
              persist_packet(mochijson2:decode(Packet))
      end,
      re:split(Data, "\r",[{return,list},trim])
     ),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    Reason1 = {tcp_error, Reason},
    {stop, Reason1, State};
handle_info(_Info, State) ->
     {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% private Function Definitions
%% ------------------------------------------------------------------

%% @doc parses packet and persist to database storage 
-spec persist_packet({'struct',[{_,_},...]}) -> any().
persist_packet({struct,[{<<"poorSignalLevel">>,Level}]}) ->
    PoorSignal = #poorsignal{
      timestamp = utils:timestamp(erlang:now()), level = Level
     },
    database_ser:insert(PoorSignal);
persist_packet({struct,[{<<"blinkStrength">>,Strength}]}) ->
    Blink = #blink{
      timestamp = utils:timestamp(erlang:now()), strength = Strength
     },
    database_ser:insert(Blink);
persist_packet(
  {struct,[
           {<<"eSense">>,
            {struct, EsenseTuples1}
           },
           {<<"eegPower">>,
            {struct, EsenseTuples2}
           },
           {<<"poorSignalLevel">>,PoorSignalLevel}
          ]
  }) ->
    Timestamp = utils:timestamp(erlang:now()),
    PoorSignal = #poorsignal{ timestamp = Timestamp, level = PoorSignalLevel },
    database_ser:insert(PoorSignal),
    EsensesDict = dict:from_list(EsenseTuples1 ++ EsenseTuples2),
    dict:map(fun (E, Strength) ->
                     Esense = #esense{
                       timestamp = Timestamp,
                       name = erlang:binary_to_list(E),
                       strength = Strength
                      },
                     database_ser:insert(Esense)
             end,
             EsensesDict
            ).
