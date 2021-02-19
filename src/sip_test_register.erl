%%%-------------------------------------------------------------------
%%% @author abayaubakirov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(sip_test_register).
-behaviour(gen_server).
%% API
-export(
[
  start_link/0,
  register/1,
  get_user/1
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
  Table = ets:new(register_table, [set, named_table]),
  {ok,Table}.    % State is our socket


handle_call({get_user,User},_From,Table) ->
  Description1 =
    case ets:lookup(Table, User) of
      [{User, Description}] -> Description;
      [] -> none
    end,
  {reply, Description1, Table};
handle_call(_Request,_From,State) ->
  {reply, ok, State}.


handle_cast({register,{AOR,Description}},Table) ->
  {FormIp,FromPort} = Description,
  io:fwrite("registered: ~p ,IP: ~p, Port: ~p ~n", [AOR,FormIp,FromPort]),
  true = ets:insert(Table, {AOR, Description}),
  {noreply, Table};
handle_cast(_,State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% ====================================================================
%% API
%% ====================================================================

register({AOR,Description})->
  gen_server:cast(?MODULE,{register,{AOR,Description}}),
  ok.

get_user(AOR)->
  gen_server:call(?MODULE,{get_user,AOR}).