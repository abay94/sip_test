%%%-------------------------------------------------------------------
%%% @author abayaubakirov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(sip_test_util).
%% API
-export(
[
  parse/1
]).



parse({udp,_UdpPort,_FormIp, _FromPort, <<"SIP/2.0 200 OK", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  AOR = get_name(SplitMsg),
  case sip_test_register:get_user(AOR) of
    none -> thereisno_account;
    {IP,PORT} ->
      ok = sip_test_srv:response({IP, PORT,Msg})
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"SIP/2.0 180 Ringing", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  AOR = get_name(SplitMsg),
  case sip_test_register:get_user(AOR) of
  none -> thereis_no_account;
  {IP,PORT} ->
  ok = sip_test_srv:response({IP, PORT,Msg})
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"INVITE ", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  AOR = get_name(SplitMsg),
  case sip_test_register:get_user(AOR) of
    none -> thereisno_account;
    {IP,PORT} ->
      ok = sip_test_srv:invite({IP, PORT,Msg})
  end;

parse({udp,_UdpPort,FormIp, FromPort, <<"REGISTER ", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  ResponseList = [<<"SIP/2.0 200 OK">> | SplitMsg],
  AOR = get_name(SplitMsg),
  Response = lists:foldl(
    fun(El,Acc) ->
      <<Acc/binary,El/binary,"\r\n">>
    end,
    <<"">>,ResponseList),
  ok = sip_test_register:register({AOR,{FormIp,FromPort}}),
  ok = sip_test_srv:reply_register_ok({FormIp, FromPort,Response});
parse(_Msg) ->
  ok.



get_name([])->none;
get_name([<<"From:", Cont/binary>>|_])->
  [H|_] = binary:split(Cont,<<">">>,[global]),
  [_|[AOR]] = binary:split(H,<<"<">>,[global]),
  <<"<",AOR/binary,">">>;
get_name([_|Rest])->
  get_name(Rest).


