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
  {User,_Ip,_Port} = get_contact(SplitMsg),
  case sip_test_register:get_user(User) of
    none -> thereisno_account;
    {IP,PORT} ->
      ok = sip_test_srv:send({IP, PORT, Msg})
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"SIP/2.0 180 Ringing", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  {User,_Ip,_Port} = get_contact(SplitMsg),
  case sip_test_register:get_user(User) of
    none -> thereis_no_account;
    {IP,PORT} ->
      ok = sip_test_srv:send({IP, PORT,Msg})
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"INVITE ", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  {User,_Ip,_Port} = get_contact(SplitMsg),
  case sip_test_register:get_user(User) of
    none -> thereisno_account;
    {IP,PORT} ->
      ok = sip_test_srv:send({IP, PORT,Msg})
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"REGISTER ", _Rest/binary>>=Msg}) ->
  [_|SplitMsg] = binary:split(Msg,<<"\r\n">>,[global]),
  ResponseList = [<<"SIP/2.0 200 OK">> | SplitMsg],
  {User,Ip,Port} =  get_contact(SplitMsg),
  Response = lists:foldl(
    fun(El,Acc) ->
      <<Acc/binary,El/binary,"\r\n">>
    end,
    <<"">>,ResponseList),
  ok = sip_test_register:register({User,{Ip,Port}}),
  ok = sip_test_srv:send({Ip, Port,Response});
parse(_Msg) ->
  ok.



get_contact([])->none;
get_contact([<<"Contact:", Cont/binary>>|_])->
  [H|_] = binary:split(Cont,<<">">>,[global]),
  [_|[C]] = binary:split(H,<<"<">>,[global]),
  [User|[D]] = binary:split(C,<<"@">>,[global]),
  [IpPort|_] = binary:split(D,<<";">>,[global]),
  [Ip|[Port]] = binary:split(IpPort,<<":">>,[global]),
  [IP1,IP2,IP3,IP4|_] = binary:split(Ip,<<".">>,[global]),
  {User,{list_to_integer(binary_to_list(IP1)),list_to_integer(binary_to_list(IP2)),list_to_integer(binary_to_list(IP3)),list_to_integer(binary_to_list(IP4))},list_to_integer(binary_to_list(Port))};
get_contact([_|Rest])->
  get_contact(Rest).


