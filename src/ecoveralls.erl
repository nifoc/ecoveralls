% Copyright (c) 2014, Daniel Kempkens <daniel@kempkens.io>
%
% Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted,
% provided that the above copyright notice and this permission notice appear in all copies.
%
% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
% WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
% DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
% NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ecoveralls).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(COVERALLS_URL, "https://coveralls.io/api/v1/jobs").

% Types

-type option() :: {service_job_id, binary()} | {service_name, binary()} | {src_dirs, [string()]}.
-type line_coverage() :: non_neg_integer() | null.

-type options() :: [option()].
-type file_coverage() :: [{binary(), binary() | [line_coverage()]}].

-export_type([
  options/0,
  file_coverage/0
]).

% API
-export([
  start/0,
  stop/0,
  run/1,
  run/2,
  report/1,
  report/2
]).

% API

-spec start() -> ok.
start() ->
  ok = application:start(asn1),
  ok = application:start(crypto),
  ok = application:start(public_key),
  ok = application:start(ssl),
  ok = inets:start(),
  ok = application:start(jsx),
  ok = application:start(ecoveralls),
  ok.

-spec stop() -> ok.
stop() ->
  ok = application:stop(ecoveralls),
  ok = application:stop(jsx),
  ok = inets:stop(),
  ok = application:stop(ssl),
  ok = application:stop(public_key),
  ok = application:stop(crypto),
  ok = application:stop(asn1),
  ok.

-spec run(string()) -> jsx:json_term().
run(CoverData) ->
  run(CoverData, []).

-spec run(string(), options()) -> jsx:json_term().
run(CoverData, Options) ->
  ok = cover:import(CoverData),
  ServiceJobId = proplists:get_value(service_job_id, Options, null),
  ServiceName = proplists:get_value(service_name, Options, null),
  FileCoverage = coverage_report(cover:imported_modules(), Options),
  [{<<"service_job_id">>, ServiceJobId}, {<<"service_name">>, ServiceName}, {<<"source_files">>, FileCoverage}].

-spec report(string()) -> ok | {error, term()}.
report(CoverData) ->
  report(CoverData, []).

-spec report(string(), options()) -> ok | {error, term()}.
report(CoverData, Options) ->
  case run(CoverData, Options) of
    [] -> ok;
    Data ->
      JsonData = jsx:encode(Data),
      case httpc:request(post, {?COVERALLS_URL, [], "application/json", JsonData}, [], []) of
        {ok, _Result} -> ok;
        {error, Reason} ->
          io:format(user, "~p~n", [Reason]),
          ok
      end
  end.

% Private

-spec coverage_report([module()], options()) -> [file_coverage()].
coverage_report(Modules, Options) ->
  coverage_report(Modules, Options, []).

-spec coverage_report([module()], options(), [file_coverage()]) -> [file_coverage()].
coverage_report([], _Options, Acc) ->
  Acc;
coverage_report([Mod|Rest], Options, Acc) ->
  case code:where_is_file(atom_to_list(Mod) ++ ".beam") of
    non_existing -> coverage_report(Rest, Options, Acc);
    BeamFile when is_list(BeamFile) ->
      FileCoverage = file_coverage(Mod, BeamFile, Options),
      coverage_report(Rest, Options, [FileCoverage | Acc])
  end.

-spec file_coverage(module(), string(), options()) -> file_coverage().
file_coverage(Mod, BeamFile, Options) ->
  SourcePaths = proplists:get_value(src_dirs, Options, ["src"]),
  SourceFile = find_source_file(BeamFile, SourcePaths),
  {ok, Source} = file:read_file(SourceFile),
  SourceLines = binary:split(Source, <<"\n">>, [global]),
  {ok, Coverage} = cover:analyse(Mod, calls, line),
  CoverageLines = line_coverage(1, length(SourceLines), maybe_drop_first(Coverage), []),
  Filename = relative_source_file_path(SourceFile),
  [{<<"name">>, unicode:characters_to_binary(Filename)}, {<<"source">>, Source}, {<<"coverage">>, CoverageLines}].

-spec find_source_file(string(), [string()]) -> string().
find_source_file(BeamFile, SourcePaths) ->
  BeamDir = filename:dirname(BeamFile),
  SrcFilename = re:replace(filename:basename(BeamFile), "\.beam$", ".erl", [{return, list}]),
  SrcFiles = lists:map(fun(SourcePath) ->
    SrcDir = filename:join([BeamDir, "..", SourcePath]),
    filename:join([SrcDir, SrcFilename])
  end, SourcePaths),
  ExistingSrcFiles = lists:filter(fun(F) -> filelib:is_file(F) end, SrcFiles),
  hd(ExistingSrcFiles).

-spec maybe_drop_first(list()) -> list().
maybe_drop_first([]) -> [];
maybe_drop_first([{{_Mod, 0}, _Calls}|Rest]) -> Rest;
maybe_drop_first(List) -> List.

-spec relative_source_file_path(string()) -> string().
relative_source_file_path(SrcFile) ->
  Path = tl(string:tokens(SrcFile, ".")),
  tl(string:join(Path, ".")).

-spec line_coverage(pos_integer(), pos_integer(), list(), line_coverage()) -> line_coverage().
line_coverage(Line, EndLine, _Coverage, Acc) when Line > EndLine ->
  lists:reverse(Acc);
line_coverage(Line, EndLine, [], Acc) ->
  line_coverage(Line + 1, EndLine, [], [null | Acc]);
line_coverage(Line, EndLine, [{{_Mod, Line}, Calls}|Rest], Acc) ->
  line_coverage(Line + 1, EndLine, Rest, [Calls | Acc]);
line_coverage(Line, EndLine, Coverage, Acc) ->
  line_coverage(Line + 1, EndLine, Coverage, [null | Acc]).

% Tests (private functions)

-ifdef(TEST).
find_source_file_test() ->
  BeamFile = code:where_is_file(?MODULE_STRING ++ ".beam"),
  SrcFile = find_source_file(BeamFile, ["src"]),
  ?assertEqual("src/ecoveralls.erl", relative_source_file_path(SrcFile)).

maybe_drop_first_test() ->
  ?assertEqual([], maybe_drop_first([])),
  ?assertEqual([1], maybe_drop_first([1])),
  ?assertEqual([], maybe_drop_first([{{test, 0}, 9001}])).
-endif.
