%%------------------------------------------------------------------------------
%% @doc utils
%%
%% Utils module is in charge of offer helper functions for all other modules.
%%
%% @copyright 2017 Alert Logic, Inc.
%%------------------------------------------------------------------------------
-module(utils).

-ifdef(TEST).
-compile([export_all]).
-endif.

%% Datetime and timer helpers
-export([sleep/2, get_interval/2, get_interval/3, get_time_difference/2, get_time_difference/3]).

%% String helpers
-export([binary_join/2, need_atom/1, need_binary/1, atom_join/1, atom_join/2, need_integer/1, need_list/1]).
-export([list_join/1, list_join/2, need_positive/1]).
%% Other utilities
-export([get_attribute/2, parse_ip/1, normalize_body/1, calc_size/1, get_geo_location/1]).
-export([debug/3, debug/2, debug/1, warning/2, warning/1, err/1, err/2, inf/1, inf/2]).
-export([division/2, division/3, default/2, get_module_info/1]).
%% List utilities
-export([list_find/2, rem_all_occurrences/2, find/2, keys/1]).


-spec get_attribute(Key :: binary(), #{}) -> binary() | tuple() | number() | undefined.
%%----------------------------------------------------------------------
%% @doc Gets an attribute from a list, otherwise returns undefined
%%----------------------------------------------------------------------
get_attribute(Key, Attributes) ->
  case maps:get(Key, Attributes, undefined) of
    null -> undefined;
    R -> R
  end.

-spec binary_join(List :: list(), _Sep :: any()) -> binary().
%%----------------------------------------------------------------------
%% @doc Joins a list of binaries
%%----------------------------------------------------------------------
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun(A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
              end, <<>>, List).

-spec sleep(Time :: integer(), _Type :: atom()) -> ok.
%%----------------------------------------------------------------------
%% @doc Uses timer:sleep according to _Type, defaults to hours
%%----------------------------------------------------------------------
sleep(Time, _Type) ->
  debug("Sleep ~p ~p...", [Time, _Type]),
  Sleep = case _Type of
            minutes -> timer:minutes(Time);
            seconds -> timer:seconds(Time);
            _ -> timer:hours(Time)
          end,
  timer:sleep(round(Sleep)).


-spec get_interval(Interval :: integer(), _Type :: atom()) -> integer().
%%----------------------------------------------------------------------
%% @doc Gets an interval of time by date
%%----------------------------------------------------------------------
get_interval(Interval, Type) ->
  get_interval(qdate:unixtime(), Interval, Type).

-spec get_interval(
    Timestamp :: integer(), Interval :: integer(), Type :: atom()
) -> integer().
%%----------------------------------------------------------------------
%% @doc Gets an interval of time by date
%%----------------------------------------------------------------------
get_interval(Timestamp, Interval, Type) ->
  case Type of
    minutes -> qdate:add_minutes(-(Interval), Timestamp);
    seconds -> qdate:add_seconds(-(Interval), Timestamp);
    days -> qdate:add_days(-(Interval), Timestamp);
    _ -> qdate:add_hours(-(Interval), Timestamp)
  end.

-spec parse_ip(IpAddress :: binary()) -> binary().
%%----------------------------------------------------------------------
%% @doc Validates an ipv4, returns <<"0.0.0.0">> if invalid
%%----------------------------------------------------------------------
parse_ip(IpAddress) ->
  case inet_parse:address(need_list(IpAddress)) of
    {error, einval} ->
      <<"0.0.0.0">>;
    {ok, _Ip} -> parse_ip(_Ip, IpAddress)
  end.

-spec parse_ip(IpAddress :: binary(), OldIpAddress :: binary()) -> binary().
%%----------------------------------------------------------------------
%% @doc Validates an ipv4, returns <<"0.0.0.0">> if invalid
%%----------------------------------------------------------------------
parse_ip(Parsed, OldIpAddress) ->
  case validate_ipv4(Parsed) of
    true -> OldIpAddress;
    false -> <<"0.0.0.0">>
  end.

-spec validate_ipv4(Ip :: list()) -> binary().
%%----------------------------------------------------------------------
%% @doc Validates an ipv4, returns <<"0.0.0.0">> if invalid
%%----------------------------------------------------------------------
validate_ipv4(Ip) when (is_tuple(Ip)) ->
  validate_ipv4(tuple_to_list(Ip));
validate_ipv4(Ip) when (length(Ip) =/= 4) ->
  false;
validate_ipv4(Ip) when (length(Ip) =:= 4) ->
  true.

-spec need_atom(Match :: any()) -> atom().
%%----------------------------------------------------------------------
%% @doc converts anything to atom
%%----------------------------------------------------------------------
need_atom(Match) when (is_list(Match)) ->
  list_to_atom(Match);
need_atom(Match) when (is_binary(Match)) ->
  binary_to_atom(Match, utf8);
need_atom(Match) when (is_atom(Match)) ->
  Match.

-spec need_binary(Match :: any()) -> list().
%%----------------------------------------------------------------------
%% @doc converts anything to binary
%%----------------------------------------------------------------------
need_binary(Match) when (is_list(Match)) ->
  list_to_binary(Match);
need_binary(Match) when (is_integer(Match)) ->
  integer_to_binary(Match);
need_binary(Match) when (is_atom(Match)) ->
  atom_to_binary(Match, utf8);
need_binary(Match) when is_float(Match) ->
  float_to_binary(Match);
need_binary(Match) when (is_binary(Match)) ->
  Match.

-spec need_integer(Match :: any()) -> integer().
%%----------------------------------------------------------------------
%% @doc converts anything to integer
%%----------------------------------------------------------------------
need_integer(Match) when (is_integer(Match)) orelse (is_atom(Match)) ->
  Match;
need_integer(Match) when (is_list(Match)) ->
  list_to_integer(Match);
need_integer(Match) when (is_binary(Match)) ->
  binary_to_integer(Match).


-spec need_list(Match :: any()) -> list().
%%----------------------------------------------------------------------
%% @doc converts anything to list
%%----------------------------------------------------------------------
need_list(Match) when is_list(Match) ->
  Match;
need_list(Match) when is_integer(Match) ->
  integer_to_list(Match);
need_list(Match) when is_atom(Match) ->
  atom_to_list(Match);
need_list(Match) when is_binary(Match) ->
  binary_to_list(Match);
need_list(Match) when is_float(Match) ->
  float_to_list(Match);
need_list(Match) when is_tuple(Match) ->
  tuple_to_list(Match).


-spec need_positive(term()) -> integer().
%%----------------------------------------------------------------------
%% @doc Returns a positive number
%%----------------------------------------------------------------------
need_positive(Match) when Match =:= null ->
  0;
need_positive(Match) ->
  abs(need_integer(Match)).

-spec atom_join(List :: list()) -> atom().
%%----------------------------------------------------------------------
%% @doc Joins to atoms and returns the corresponding atom
%%----------------------------------------------------------------------
atom_join(List) ->
  atom_join(List, "_").
atom_join(List, Separator) when Separator =/= "_"->
  Lists = atoms_to_list(List),
  string:join(Lists, Separator);
atom_join(List, Separator) ->
  Lists = atoms_to_list(List),
  list_to_atom(string:join(Lists, Separator)).

atoms_to_list(List) ->
  lists:map(
    fun(Atom) ->
      need_list(Atom)
    end, List).


-spec list_join(List :: list()) -> atom().
%%----------------------------------------------------------------------
%% @doc Joins a list of lists into one using the specified separator
%%----------------------------------------------------------------------
list_join(List) ->
  list_join(List, "_").
list_join(List, Separator) when Separator =/= "_"->
  Lists = atoms_to_list(List),
  string:join(Lists, Separator);
list_join(List, Separator) ->
  Lists = [need_list(Item)|| Item <- List ],
  list_to_atom(string:join(Lists, Separator)).


-spec get_time_difference(TimeStarted :: integer(), TimeEnded :: integer() ) -> list().
%%----------------------------------------------------------------------
%% @doc Gets the time difference between to dates
%%----------------------------------------------------------------------
get_time_difference(TimeStarted, TimeEnded) ->
  get_time_difference(TimeStarted, TimeEnded, seconds).

-spec get_time_difference(TimeStarted :: integer(), TimeEnded :: integer(), Type :: atom()) -> list().
get_time_difference(TimeStarted, TimeEnded, Type) ->
  Difference = TimeEnded - TimeStarted,
  case Type of
    minutes ->
      Difference / 60 / 60;
    seconds ->
      Difference;
    miliseconds ->
      {Mega,Sec,Micro} = Difference,
      (Mega*1000000+Sec)*1000000+Micro;
    days ->
      (Difference / 60) / 24
  end.

-spec normalize_body(Body :: binary()) -> binary().
%%----------------------------------------------------------------------
%% @doc Make sure we send a binary body with http request
%%----------------------------------------------------------------------
normalize_body(Body) when is_binary(Body) ->
  Body;
normalize_body(undefined) ->
  <<>>.


-spec calc_size(String :: binary()) -> float().
%%----------------------------------------------------------------------
%% @doc Calculate the size in bytes of a binary string
%%----------------------------------------------------------------------
calc_size(String) when is_list(String) ->
  calc_size(list_to_binary(String));
calc_size(String) ->
  Length = byte_size(String),
  Num = float_to_list(Length / 1024 / 1024, [{decimals,2}]),
  list_to_float(Num).


-spec get_geo_location(
    Ip :: binary()
) -> list().
%%----------------------------------------------------------------------
%% @doc Gets geoip location for an IP address
%%----------------------------------------------------------------------
get_geo_location(Ip) ->
  case egeoip:lookup_pl(parse_ip(Ip)) of
    {error, _} ->
      <<"">>;
    GeoIp ->
      Lat = proplists:get_value(latitude, GeoIp),
      Long = proplists:get_value(longitude, GeoIp),
      utils:list_join([Lat, Long], ",")
  end.

-spec debug(Message :: list()) -> ok.
%%----------------------------------------------------------------------
%% @doc Wrapper of al_logger for debug
%%----------------------------------------------------------------------
debug(Message) ->
  debug(Message, []).
debug(Message, Args) ->
  debug(Message, Args, debug).
debug(Message, Args, Type) ->
  case Type of
    debug ->
      error_logger:debug_msg(Message, Args);
    warning ->
      error_logger:warning_msg(Message, Args);
    error ->
      error_logger:error_msg(Message, Args);
    info ->
      error_logger:info_msg(Message, Args)
  end.

-spec warning(Message :: list()) -> ok.
%%----------------------------------------------------------------------
%% @doc Wrapper of error_logger for warning
%%----------------------------------------------------------------------
warning(Message) ->
  warning(Message, []).
warning(Message, Args) ->
  debug(Message, Args, warning).

-spec err(Message :: list()) -> ok.
err(Message) ->
  err(Message, []).
err(Message, Args) ->
  debug(Message, Args, error).


-spec inf(Message :: list()) -> ok.
%%----------------------------------------------------------------------
%% @doc Wrapper of al_logger for info
%%----------------------------------------------------------------------
inf(Message) ->
  inf(Message, []).
inf(Message, Args) ->
  debug(Message, Args, info).


-spec division(X :: integer(), Y :: integer()) -> integer().
%%----------------------------------------------------------------------
%% @doc Safely divide 2 integers, avoid division by zero
%%----------------------------------------------------------------------
division(X, Y) ->
  division(X,Y, 0).
division(X, Y, Default) ->
  case catch X / Y of
    {'EXIT', _} -> Default;
    _Any -> _Any
  end.

-spec default(Item :: any(), Y :: any()) -> any().
%%----------------------------------------------------------------------
%% @doc Compares any to not null or undefined and returns the default value
%%----------------------------------------------------------------------
default(Item, Default) when Item =:= 0 ->
  Default;
default(Item, Default) when Item =:= null ->
  Default;
default(Item, Default) when Item =:= undefined ->
  Default;
default(Item, _Default) ->
  Item.

get_module_info(Mod) when Mod =:= undefined ->
  false;
get_module_info(Mod) ->
  try
    (Mod):module_info()
  catch
    _:_  ->
      utils:err("Module ~p doesn't exist, please check rest_handler param in your config", [Mod]),
      false
  end.

list_find ( _Element, [] ) ->
  false;

list_find ( Element, [ Item | ListTail ] ) ->
  case ( Item == Element ) of
    true    ->  true;
    false   ->  list_find(Element, ListTail)
  end.


find(_, []) -> false;

find(E, T) when is_tuple(T) ->
  find(E, tuple_to_list(T));

find(E, [H|T]) ->
  case find(E, H) of
    false -> find(E, T);
    true -> true
  end;

find(V, E) -> V == E.

rem_all_occurrences(Elem, List) ->
  [E || E <- List, E =/= Elem].

keys(TableName) ->
  FirstKey = ets:first(TableName),
  keys(TableName, FirstKey, [FirstKey]).

keys(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
  Acc;
keys(TableName, CurrentKey, Acc) ->
  NextKey = ets:next(TableName, CurrentKey),
  keys(TableName, NextKey, [NextKey|Acc]).