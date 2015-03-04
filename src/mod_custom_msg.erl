-module(mod_custom_msg).
-author('trepa@malkosua.com').

-behavior(gen_mod).

-export([start/2, stop/1]).
-export([send_like/6, send_flag/5, send_expire/5]).

%%-include("ns.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").

-define(SendFrom, "admin@localhost"). %% Edit this config constant

start(_Host, _Opts) ->
	error_logger:info_msg("mod_like_msg starting", []),
  	ejabberd_commands:register_commands(commands()).

stop(_Host) ->
	error_logger:info_msg("mod_like_msg stopping", []),
	ejabberd_commands:unregister_commands(commands()).	

commands() ->
    [
	#ejabberd_commands{name = send_like, tags = [custom_msg],
		desc = "Send notify about liked message",
		module = ?MODULE, function = send_like,
		args = [{to, string}, {msg_id, string}, {status, string}, {username, string}, {timestamp, string}, {jid, string}],
		result = {res, rescode}},
	#ejabberd_commands{name = send_flag, tags = [custom_msg],
		desc = "Send notify about flagged message",
		module = ?MODULE, function = send_flag,
		args = [{to, string}, {msg_id, string}, {status, string}, {username, string}, {timestamp, string}],
		result = {res, rescode}},
    #ejabberd_commands{name = send_expire, tags = [custom_msg],
		desc = "Send notify about expired room",
		module = ?MODULE, function = send_expire,
		args = [{to, string}, {name, string}, {jid, string}, {status, string}, {timestamp, string}],
		result = {res, rescode}}
    ].

send_like(To, MsgId, Status, Username, Timestamp, Jid) ->
	Packet = build_packet(message_like, [MsgId, Status, Username, Timestamp, Jid]),
	send_packet_all_resources(?SendFrom, To, Packet).

send_flag(To, MsgId, Status, Username, Timestamp) ->
	Packet = build_packet(message_flag, [MsgId, Status, Username, Timestamp]),
	send_packet_all_resources(?SendFrom, To, Packet).

send_expire(To, Name, Jid, Status, Timestamp) ->
	Packet = build_packet(message_expire, [Name, Jid, Status, Timestamp]),
	send_packet_all_resources(?SendFrom, To, Packet).

build_packet(message_like, [MsgId, Status, Username, Timestamp, Jid]) ->
	{xmlelement, "presence",
		[{"type", "msg_like"}],
		[{xmlelement, "item", [{"msg_id", MsgId}, {"status", Status}, {"username", Username}, {"timestamp", Timestamp}, {"jid", Jid}], []}]
	};
build_packet(message_flag, [MsgId, Status, Username, Timestamp]) ->
	{xmlelement, "presence",
		[{"type", "msg_flag"}],
		[{xmlelement, "item", [{"msg_id", MsgId}, {"status", Status}, {"username", Username}, {"timestamp", Timestamp}], []}]
	};
build_packet(message_expire, [Name, Jid, Status, Timestamp]) ->
	{xmlelement, "presence",
		[{"type", "msg_expire"}],
		[{xmlelement, "item", [{"name", Name}, {"jid", Jid}, {"status", Status}, {"timestamp", Timestamp}], []}]
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