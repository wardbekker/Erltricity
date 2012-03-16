%% @author Ward Bekker <ward@equanimity.nl>
%% @doc
%% Webmachine resource for event entry form
%% @end
-module(entry_resource).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([init/1, to_html/2, allowed_methods/2, process_post/2]).

%% ------------------------------------------------------------------
%% Header includes
%% ------------------------------------------------------------------

-include_lib("webmachine/include/webmachine.hrl").
-include("erltricity.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

init([]) -> {ok, undefined}.

allowed_methods(ReqData, State) ->
    {['GET', 'HEAD', 'POST'], ReqData, State}.

process_post(ReqData, State) ->
    [{"event", Event}] = mochiweb_util:parse_qs(wrq:req_body(ReqData)),
    EventRec = #event{
      timestamp = utils:timestamp(erlang:now()), name = Event
     },
    database_ser:insert(EventRec),
    {true, ReqData, State}.

to_html(ReqData, State) -> 
    Data = [],
    {ok, Content} = entry_dtl:render(Data),
    {Content, ReqData, State}.

