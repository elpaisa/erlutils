%%------------------------------------------------------------------------------
%% @doc utils_tests
%%
%% utilities tests
%%
%% @copyright 2017 Alert Logic, Inc.
%%------------------------------------------------------------------------------
-module(utils_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_TIMEOUT, 50).

-import(utils, [sleep/2, get_interval/2, get_interval/3, get_time_difference/2, get_time_difference/3,
binary_join/2, need_atom/1, need_binary/1, atom_join/1, atom_join/2, need_integer/1,
get_attribute/2, parse_ip/1, normalize_body/1, validate_ipv4/1, need_list/1]).

setup() ->
  {[]}.

teardown({Apps}) ->
  meck:unload(Apps),
  ok.


all_test_() ->
  {
    setup,
    fun setup/0,
    fun teardown/1,
    [
      {"utils", {timeout, ?TEST_TIMEOUT, fun test_utils/0}}
    ]
  }.


test_utils() ->
  EndTime = 1490718479,
  StartTime = qdate:add_days(-1, EndTime),
  [
    ?assertEqual(
      ok,
      sleep(1, seconds)
    ),
    ?assertEqual(
      StartTime,
      get_interval(EndTime, 1, days)
    ),
    ?assertEqual(
      <<"127.0.0.0">>,
      parse_ip(<<"127.0.0.0">>)
    ),
    ?assertEqual(
      <<"0.0.0.0">>,
      parse_ip(<<"invalid-ip">>)
    ),
    ?assertEqual(
      false,
      validate_ipv4("invalid-ip")
    ),
    ?assertEqual(
      true,
      validate_ipv4({127, 0, 0, 1})
    ),
    ?assertEqual(
      none,
      need_atom(<<"none">>)
    ),
    ?assertEqual(
      none,
      need_atom("none")
    ),
    ?assertEqual(
      none,
      need_atom(none)
    ),

    ?assertEqual(
      <<"none">>,
      need_binary(none)
    ),
    ?assertEqual(
      <<"none">>,
      need_binary("none")
    ),
    ?assertEqual(
      2,
      need_integer(<<"2">>)
    ),
    ?assertEqual(
      2,
      need_integer("2")
    ),
    ?assertEqual(
      2,
      need_integer(2)
    ),
    ?assertEqual(
      "bin example",
      need_list(<<"bin example">>)
    ),
    ?assertEqual(
      test_undef,
      atom_join([test, undef])
    ),
    ?assertEqual(
      test_undef,
      atom_join(["test", "undef"])
    ),
    ?assertEqual(
      test_undef,
      atom_join([<<"test">>, <<"undef">>])
    ),
    ?assertEqual(
      7,
      get_time_difference(1490719818, 1490719825, seconds)
    ),
    ?assertEqual(
      <<"1234">>,
      normalize_body(<<"1234">>)
    ),
    ?assertEqual(<<>>, normalize_body(undefined))
  ].


-endif.