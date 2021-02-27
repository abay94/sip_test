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
  change_udp_port/1
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

-record(state, {udp, call_ids=[]}).

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
  ?LOGINFO("Here is MSG: ~p ",[Msg]),
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

change_udp_port(Port) ->
  gen_server:cast(?MODULE,{change_udp_port,Port}),
  ok.