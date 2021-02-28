%%%-------------------------------------------------------------------
%%% @author abayaubakirov
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%%-------------------------------------------------------------------
-module(sip_test_util).
-include("sip_test.hrl").
%% API
-export(
[
  parse/1
]).



parse({udp,_UdpPort,_FormIp, _FromPort, <<"BYE ", _Rest/binary>>=Msg}) ->
  SplitMsg = binary:split(Msg,<<"\r\n">>,[global]),
  User = get_user_to(SplitMsg),
  case sip_test_register:get_user(User) of
    none -> thereis_no_account;
    {IP,PORT} ->
      ok = sip_test_srv:send_bye({IP, PORT}, Msg)
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"ACK ", _Rest/binary>>=Msg}) ->
  SplitMsg = binary:split(Msg,<<"\r\n">>,[global]),
  CallId = get_call_id(SplitMsg),
  ok = sip_test_srv:send_ack(CallId,Msg);

%% Ok response for INVITE
parse({udp,_UdpPort,_FormIp, _FromPort, <<"SIP/2.0 200 Ok", _Rest/binary>>=Msg}) ->
  SplitMsg = binary:split(Msg,<<"\r\n">>,[global]),
  SwappedMsg = swap_contact_to_server(SplitMsg,[]),
  CallId = get_call_id(SplitMsg),
  ok = sip_test_srv:send_ok(CallId,SwappedMsg);

%%End OK response for BYE
parse({udp,_UdpPort,_FormIp, _FromPort, <<"SIP/2.0 200 OK", _Rest/binary>>=Msg}) ->
  SplitMsg = binary:split(Msg,<<"\r\n">>,[global]),
  User = get_user_from(SplitMsg),
  CallId = get_call_id(SplitMsg),
  case sip_test_register:get_user(User) of
    none -> thereisno_account;
    {IP,PORT} ->
      ok = sip_test_srv:send_bye_ok({IP,PORT},CallId,Msg)
  end;

parse({udp,_UdpPort,_FormIp, _FromPort, <<"SIP/2.0 180 Ringing", _Rest/binary>>=Msg}) ->
  SplitMsg = binary:split(Msg,<<"\r\n">>,[global]),
  CallId = get_call_id(SplitMsg),
  ok = sip_test_srv:send_ringing(CallId,Msg);

parse({udp,_UdpPort,_FormIp, _FromPort, <<"INVITE ", _Rest/binary>>=Msg}) ->
  SplitMsg = binary:split(Msg,<<"\r\n">>,[global]),
  User = get_user_to(SplitMsg),
  CallId = get_call_id(SplitMsg),
  {_,IpFrom,PortFrom} =  get_contact(SplitMsg),
  SwappedMsg = swap_contact_to_server(SplitMsg,[]),
  case sip_test_register:get_user(User) of
    none -> thereisno_account;
    {IP,PORT} ->
      ok = sip_test_srv:send_invite({{IpFrom,PortFrom},{IP, PORT}, SwappedMsg},CallId)
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


swap_contact_to_server([],Acc)->
  lists:foldl(
    fun(El,Acc1) ->
      <<Acc1/binary,El/binary,"\r\n">>
    end,
    <<"">>,lists:reverse(Acc));
swap_contact_to_server([<<"Contact: ", _/binary>>|Rest],Acc)->
  C = <<"Contact: \" Server \" <sip:server@127.0.0.1:5060> ">>,
  swap_contact_to_server(Rest,[C|Acc]);
swap_contact_to_server([L|Rest],Acc)->
  swap_contact_to_server(Rest,[L|Acc]).

get_call_id([])->none;
get_call_id([<<"Call-ID: ", CallId/binary>>|_])->
  CallId;
get_call_id([_|Rest])->
  get_call_id(Rest).

get_user_from([])->none;
get_user_from([<<"From: ", Cont/binary>>|_])->
  [A|_] = binary:split(Cont,<<"@">>,[global]),
  [_|[U]] = binary:split(A,<<":">>,[global]),
  <<"sip:",U/binary>>;
get_user_from([_|Rest])->
  get_user_from(Rest).

get_user_to([])->none;
get_user_to([<<"To: ", Cont/binary>>|_])->
  [A|_] = binary:split(Cont,<<"@">>,[global]),
  [_|[U]] = binary:split(A,<<":">>,[global]),
  <<"sip:",U/binary>>;
get_user_to([_|Rest])->
  get_user_to(Rest).



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



