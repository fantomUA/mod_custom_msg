-module(mod_custom_msg).
-author('trepa@malkosua.com').

-behavior(gen_mod).

-export([start/2, stop/1]).
-export([send_like/4]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").

start(_Host, Opts) ->
	?INFO_MSG("mod_like_msg starting", []),
	SendFrom = gen_mod:get_opt(send_from, Opts, ""),
	ets:new(custom_msg, [named_table, protected, set, {keypos, 1}]),
  	ets:insert(custom_msg, {send_from, SendFrom}),
  	ejabberd_commands:register_commands(commands()).

stop(_Host) ->
	?INFO_MSG("mod_like_msg stopping", []),
	ejabberd_commands:unregister_commands(commands()).	

commands() ->
    [
	#ejabberd_commands{name = send_like, tags = [like],
		desc = "Send notifikations about liked message",
		module = ?MODULE, function = send_like,
		args = [{to, string}, {msg_id, string}, {status, string}, {username, string}],
		result = {res, rescode}}
    ].

send_like(To, MsgId, Status, Username) ->
	Packet = build_packet(message_like, [MsgId, Status, Username]),
	[{_, SendFrom}] = ets:lookup(custom_msg, send_from),
	send_packet_all_resources(SendFrom, To, Packet).

build_packet(message_like, [MsgId, Status, Username]) ->
	{xmlelement, "presence",
		[{"type", "msg_like"}],
		[{xmlelement, "item", [{"msg_id", MsgId}, {"status", Status}, {"username", Username}], []}]
	}.

send_packet_all_resources(FromJIDString, ToJIDString, Packet) ->
	FromJID = jlib:string_to_jid(FromJIDString),
	ToJID = jlib:string_to_jid(ToJIDString),
	ToUser = ToJID#jid.user,
	ToServer = ToJID#jid.server,
	case ToJID#jid.resource of
	"" ->
		send_packet_all_resources(FromJID, ToUser, ToServer, Packet);
	Res ->
		send_packet_all_resources(FromJID, ToUser, ToServer, Res, Packet)
	end.

send_packet_all_resources(FromJID, ToUser, ToServer, Packet) ->
	case ejabberd_sm:get_user_resources(ToUser, ToServer) of
		[] ->
			send_packet_all_resources(FromJID, ToUser, ToServer, "", Packet);
		ToResources ->
			lists:foreach(
				fun(ToResource) ->
					send_packet_all_resources(FromJID, ToUser, ToServer, ToResource, Packet)
				end,
			ToResources)
	end.

send_packet_all_resources(FromJID, ToU, ToS, ToR, Packet) ->
	ToJID = jlib:make_jid(ToU, ToS, ToR),
	ejabberd_router:route(FromJID, ToJID, Packet).