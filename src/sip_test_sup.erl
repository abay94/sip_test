%%%-------------------------------------------------------------------
%% @doc sip_test top level supervisor.
%% @end
%% Author: Aubakirov Abay, ae.aubakirov@gmail.com
%%%-------------------------------------------------------------------

-module(sip_test_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(STOP_TIMEOUT,60000).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags =
    #{
      strategy => rest_for_one,
      intensity => 0,
      period => 1
    },
  SipSrv=#{
    id=>sip_test_srv,
    start=>{sip_test_srv,start_link,[]},
    restart=>permanent,
    shutdown=>?STOP_TIMEOUT,
    type=>worker,
    modules=>[sip_test_srv]
  },
  SipRegister=#{
    id=>sip_test_register,
    start=>{sip_test_register,start_link,[]},
    restart=>permanent,
    shutdown=>?STOP_TIMEOUT,
    type=>worker,
    modules=>[sip_test_register]
  },
  {ok, {SupFlags, [
    SipSrv,
    SipRegister
  ]}}.

%% internal functions
