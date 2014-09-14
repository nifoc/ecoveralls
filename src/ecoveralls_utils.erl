% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ecoveralls_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% API
-export([
  source_file/1,
  filename_with_path/1,
  merge_options/2
]).

% API

-spec source_file(module()) -> {ok, string()} | {error, term()}.
source_file(Mod) ->
  try Mod:module_info(compile) of
    Info ->
      Source = proplists:get_value(source, Info),
      {ok, Source}
  catch
    error:undef -> {error, source_not_found}
  end.

-spec filename_with_path(string()) -> binary().
filename_with_path(SrcFile) ->
  {ok, Cwd} = file:get_cwd(),
  Cwd2 = re:replace(Cwd, "/(logs|\.eunit)/.+$", "", [{return, list}]),
  Path = string:substr(SrcFile, length(Cwd2) + 2),
  unicode:characters_to_binary(Path).

-spec merge_options(ecoveralls:options(), ecoveralls:options()) -> ecoveralls:options().
merge_options(ListA, ListB) ->
  DictA = orddict:from_list(ListA),
  DictB = orddict:from_list(ListB),
  MergedDict = orddict:merge(fun(_Key, _ValueA, ValueB) -> ValueB end, DictA, DictB),
  orddict:to_list(MergedDict).

% Tests (private functions)

-ifdef(TEST).
source_file_test() ->
  {ok, SrcFile} = source_file(?MODULE),
  ?assertEqual(<<"src/ecoveralls_utils.erl">>, filename_with_path(SrcFile)),
  ?assertEqual({error, source_not_found}, source_file(this_does_not_exist)).

merge_options_test() ->
  ?assertEqual([{service_name, <<"test">>}], merge_options([], [{service_name, <<"test">>}])),
  ?assertEqual([{service_name, <<"test">>}], merge_options([{service_name, <<"foo">>}], [{service_name, <<"test">>}])),
  ?assertEqual([{service_job_id, <<"123">>}, {service_name, <<"test">>}], merge_options([{service_name, <<"test">>}], [{service_job_id, <<"123">>}])).
-endif.
