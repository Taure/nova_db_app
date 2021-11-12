-module(nova_db_app_message_controller).
-export([
         create_message/1,
         get_all/1,
         get_message/1,
         update_message/1,
         delete_message/1
        ]).

-include_lib("nova/include/nova.hrl").


create_message(#{json := #{<<"payload">> := Payload}}) ->
    UUID = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    SQL = <<"INSERT INTO message (id, payload) VALUES ($1, $2)">>,
    Json2 = json:encode(Payload, [maps, binary]),
    case pgo:query(SQL, [UUID, Json2]) of
        #{command := insert,
          num_rows := 1} -> {json, #{<<"id">> => UUID,
                                     <<"payload">> => Payload}};
        {error, Error} ->
            logger:warning("Create message failed: ~p", [Error]),
            {status, 500}
    end.

get_all(_) ->
    SQL = <<"SELECT * FROM message">>,
    case pgo:query(SQL, []) of
        #{command := select,
            rows := Rows} -> logger:warning("MessageList: ~p", [Rows]),
                             MessageList = [ #{ id => Id, payload => json:decode(Payload, [maps])} || #{id := Id,
                                                                                                        payload := Payload} <- Rows],
                             {json, 200, #{}, MessageList};
        {error, _Error} ->
            {status, 500}
        end.

get_message(#{bindings := #{<<"messageid">> := MessageId}}) ->
    SQL = <<"SELECT * FROM message WHERE id = $1">>,
    case pgo:query(SQL, [MessageId]) of
        #{command := select,
          rows := [Row]} -> {json, 200, #{}, Row};
        {error, _Error} ->
            {status, 500}
        end.

update_message(#{bindings := #{<<"messageid">> := MessageId},
                 json := #{<<"payload">> := Payload}}) ->
    SQL = <<"UPDATE message SET payload = $1 WHERE id = $2">>,
    Json2 = json:encode(Payload, [maps, binary]),
    case pgo:query(SQL, [Json2, MessageId]) of
        #{command := update,
          num_rows := 1} -> {json, 200, #{}, #{<<"id">> => MessageId,
                                             <<"payload">> => Payload}};
        {error, _Error} ->
            {status, 500}
    end.

delete_message(#{bindings := #{<<"messageid">> := MessageId}}) ->
    SQL = <<"DELETE FROM message WHERE id = $1">>,
    case pgo:query(SQL, [MessageId]) of
        #{command := delete,
          num_rows := 1} -> {status, 200};
        {error, _Error} ->
            {status, 500}
    end.