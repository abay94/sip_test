%%%-------------------------------------------------------------------
%%% @author abayaubakirov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(sip_test_srv).
-behaviour(gen_server).
%% API
-export(
[
  start_link/0,
  reply_register_ok/1,
  invite/1,
  response/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

%% ====================================================================
%% Server functions
%% ====================================================================
init([])->
  {ok, Socket} = gen_udp:open(5060, [binary, {active, true}]),
  {ok,Socket}.    % State is our socket



handle_call(_Request,_From,State) ->
  {reply, ok, State}.


handle_cast({invite,{FormIp, FromPort,Response}},State) ->
  gen_udp:send(State, FormIp, FromPort, Response),
  {noreply, State};
handle_cast({register_ok,{FormIp, FromPort,Response}},State) ->
  gen_udp:send(State, FormIp, FromPort, Response),
  {noreply, State};
handle_cast(_,State) ->
  {noreply, State}.


handle_info({udp,_UdpPort,_FormIp, _FromPort, Msg}=Request, State) ->
  sip_test_util:parse(Request),
  io:fwrite("Here is MSG: ~p ~n",[Msg]),
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

response({FormIp, FromPort,Response})->
  gen_server:cast(?MODULE,{register_ok,{FormIp, FromPort,Response}}),
  ok.

reply_register_ok({FormIp, FromPort,Response})->
  gen_server:cast(?MODULE,{register_ok,{FormIp, FromPort,Response}}),
  ok.

invite({FormIp, FromPort,Response}) ->
  gen_server:cast(?MODULE,{invite,{FormIp, FromPort,Response}}),
  ok.