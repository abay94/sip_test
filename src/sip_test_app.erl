%%%-------------------------------------------------------------------
%% @doc sip_test public API
%% @end
%%%-------------------------------------------------------------------

-module(sip_test_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sip_test_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
