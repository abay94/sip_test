%%%-------------------------------------------------------------------
%%% @author abayaubakirov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(sip_test_srv).
-behaviour(gen_server).
-include("sip_test.hrl").
%% API
-export(
[
  start_link/0,
  send/1,
  change_udp_port/1,
  send_invite/2,
  send_ringing/2,
  send_ok/2,
  send_ack/2,
  send_bye/2,
  send_bye_ok/3
]).

-define(DEFAULT_UDP_PORT, 5060).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {udp, call_ids=#{}}).

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% ====================================================================
%% Server functions
%% ====================================================================
init([])->
  UDPPort = os:getenv("UDP_PORT",application:get_env(sip_test,udp_port,?DEFAULT_UDP_PORT)),
  Socket1 =
    case gen_udp:open(UDPPort, [binary, {active, true}]) of
      {error,eaddrinuse} ->
        ?LOGERROR(
          "There is an error on opening a udp socket, PORT is in use. \n"
          "Please give an alternative port number with \"sip_test_srv:change_udp_port(Port)\".\n"
          "or restart the server by giving ENV argument \"UDP_PORT = port\" \n"
          "or you can change in config file."
        ),
        undefined;
      {ok,Socket} ->
        Socket;
      {error,Error} ->
        throw(Error)
    end,
  {ok,#state{udp=Socket1}}.    % State is our socket



handle_call(_Request,_From,State) ->
  {reply, ok, State}.


handle_cast({send_bye_ok,{IP,PORT},CallId,Msg},#state{udp=UDP,call_ids=CallIDs}=State) ->
  maps:remove(CallId,CallIDs),
  gen_udp:send(UDP, IP, PORT, Msg),
  {noreply, State};
handle_cast({send_bye,{IP, PORT}, Msg},#state{udp=UDP}=State) ->
  gen_udp:send(UDP, IP, PORT, Msg),
  {noreply, State};
handle_cast({send_ack,{CallId,Msg}},#state{udp=UDP,call_ids=CallIDs}=State) ->
  {_,{IpTo,PortTo}} = maps:get(CallId,CallIDs),
  gen_udp:send(UDP, IpTo, PortTo, Msg),
  {noreply, State};
handle_cast({send_ok,{CallId,Msg}},#state{udp=UDP,call_ids=CallIDs}=State) ->
  {{IpFrom,PortFrom},_} = maps:get(CallId,CallIDs),
  gen_udp:send(UDP, IpFrom, PortFrom, Msg),
  {noreply, State};
handle_cast({send_ringing,{CallId,Msg}},#state{udp=UDP,call_ids=CallIDs}=State) ->
  {{IpFrom,PortFrom},_} = maps:get(CallId,CallIDs),
  gen_udp:send(UDP, IpFrom, PortFrom, Msg),
  {noreply, State};
handle_cast({send_invite,{{IpFrom,PortFrom},{IpTo, PortTo}, SwappedMsg},CallId},#state{udp=UDP,call_ids=CallIDs}=State) ->
  CallIds1 = maps:put(CallId, {{IpFrom,PortFrom},{IpTo, PortTo}}, CallIDs),
  gen_udp:send(UDP, IpTo, PortTo, SwappedMsg),
  {noreply, State#state{call_ids=CallIds1}};
handle_cast({send,{FormIp, FromPort,Response}},#state{udp=UDP}=State) ->
  gen_udp:send(UDP, FormIp, FromPort, Response),
  {noreply, State};
handle_cast({change_udp_port,Port},#state{udp=undefined}=State) ->
  UDPSocket =
    case gen_udp:open(Port, [binary, {active, true}]) of
      {error,Error} ->
        ?LOGERROR(
          "There is an error on opening a udp port: ~p \n"
          "Please give an another port."
          ,[Error]),
        undefined;
      {ok,Socket} ->
        ?LOGINFO("You have successfully changed the port of UDP to ~p",[Port]),
        Socket
    end,
  {noreply, State#state{udp=UDPSocket}};
handle_cast({change_udp_port,Port},#state{udp=UDP}=State) ->
  ?LOGWARNING("It is going to close udp socket: ~p and opening a new port ~p",[UDP,Port]),
  ok = gen_udp:close(UDP),
  UDPSocket =
    case gen_udp:open(Port, [binary, {active, true}]) of
      {error,Error} ->
        ?LOGERROR(
          "There is an error on opening a udp port: ~p \n"
          "Please give an another port."
          ,[Error]),
        undefined;
      {ok,Socket} ->
        ?LOGINFO("You have successfully changed the port of UDP to ~p",[Port]),
        Socket
    end,
  {noreply, State#state{udp=UDPSocket}};
handle_cast(_,State) ->
  {noreply, State}.


handle_info({udp,_UdpPort,_FormIp, _FromPort, Msg}=Request, State) ->
  sip_test_util:parse(Request),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ====================================================================
%% API
%% ====================================================================

send({FormIp, FromPort,Response})->
  gen_server:cast(?MODULE,{send,{FormIp, FromPort,Response}}),
  ok.

send_invite({{IpFrom,PortFrom},{IP, PORT}, SwappedMsg},CallId)->
  gen_server:cast(?MODULE,{send_invite,{{IpFrom,PortFrom},{IP, PORT}, SwappedMsg},CallId}),
  ok.

send_ringing(CallId,Msg)->
  gen_server:cast(?MODULE,{send_ringing,{CallId,Msg}}),
  ok.

send_ok(CallId,Msg)->
  gen_server:cast(?MODULE,{send_ok,{CallId,Msg}}),
  ok.

send_ack(CallId,Msg)->
  gen_server:cast(?MODULE,{send_ack,{CallId,Msg}}),
  ok.

send_bye({IP, PORT}, Msg)->
  gen_server:cast(?MODULE,{send_bye,{IP, PORT}, Msg}),
  ok.

send_bye_ok({IP,PORT},CallId,Msg)->
  gen_server:cast(?MODULE,{send_bye_ok,{IP,PORT},CallId,Msg}),
  ok.

change_udp_port(Port) ->
  gen_server:cast(?MODULE,{change_udp_port,Port}),
  ok.