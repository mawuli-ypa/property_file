%%%----------------------------------------------------------------------
%%% File    : property_file_test.erl
%%% Author  :  <svg@surnet.ru>, <mawuli@mawuli.me>
%%% Purpose :
%%% Created :  2 Sep 2002 by  <svg@surnet.ru>
%%%----------------------------------------------------------------------
%%%
%%% $Id: property_file_test.erl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%%
%%% $Log: property_file_test.erl,v $
%%% Revision 1.1.1.1  2002/10/04 00:00:00  svg
%%% Imported sources
%%%
%%% Revision 1.2  2002/10/01 22:32:05  svg
%%% *** empty log message ***
%%%
%%% Revision 1.1.1.1  2002/09/04 20:19:07  svg
%%% Imported sources
%%%
%%%
-module(property_file_tests).
-author('svg@surnet.ru').
-author("Mawuli Adzaku <mawuli@mawuli.me>").
-include_lib("eunit/include/eunit.hrl").

-define(match(_Pat), fun (_Res) ->
                         case _Res of
                           _Pat -> ok;
                           _ -> false
                         end
                     end).
-define(CLIKE_MATCH,
	[{simple_list,["val1","val2","Val3","testvar1"]},
	 {'Quoted Atom',[{193,54,2,1}]},
	 {test_quote_char,["quote string\n\na"]},
	 {ipv4,[{193,54,2,1}]},
	 {netmask1,[{{193,54,2,0},4294967040}]},
	 {netmask2,[{{193,54,2,0},4294967040}]},
	 {prefix,["@prefix@"]},
	 {exec_prefix,["@exec_prefix@"]},
	 {sysconfdir,["@sysconfdir@"]},
	 {localstatedir,["@localstatedir@"]}|_]).


-define(APACHE_MATCH, [{'ServerType',["standalone"]},
		       {'ServerRoot',["/usr/local/psa/apache"]},
		       {'LockFile',["logs/httpd.lock"]},
		       {'PidFile',["logs/httpd.pid"]},
		       {'ScoreBoardFile',["logs/httpd.scoreboard"]},
		       {'ResourceConfig',["conf/srm.conf"]},
		       {'AccessConfig',["conf/access.conf"]},
		       {'ResourceConfig',["/dev/null"]},
		       {'AccessConfig',["/dev/null"]},
		       {'Timeout',[300]},
		       {'KeepAlive',["On"]},
		       {'MaxKeepAliveRequests',[1000]},
		       {'KeepAliveTimeout',[15]},
		       {'MinSpareServers',[5]}|_]).

%%%----------------------------------------------------------------------
%%% tests
%%%----------------------------------------------------------------------
property_file_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun teardown/1,
      [
       {"Parse Apache style config file",
        fun test_apache_parse/0},
       {"Parse C-like config file",
        fun test_clike_parse/0},
       {"Test Unix, DOS and Mac end lines ",
        fun test_end_line/0}
     ]
     }
    }.


%%%-------------------------------------------------------------------
%%% Setup / Cleanup
%%%-------------------------------------------------------------------
setup() ->
    ok.

teardown(_) ->
    ok.

test_clike_parse() ->
  FileConf = "./priv/test/clike.conf",
  Tests =
    [{"C-like config",
      [FileConf, clike, [{test_var1, "testvar1"}]],
      ?match(?CLIKE_MATCH)}
    ],

  ?assert(ok == process_tests(fun (F, M, E) -> property_file:parse(F, M, E) end, Tests)).

test_apache_parse() ->
  FileConf = "./priv/test/httpd.conf",
  Tests =
    [{"Apache config",
      [FileConf, apache],
      ?match(?APACHE_MATCH)}
    ],

    ?assert(ok == process_tests(fun (F, M) -> property_file:parse(F, M) end, Tests)).

test_end_line() ->
  Unix = property_file:parse("./priv/test/clike.conf", clike),
  Dos  = property_file:parse("./priv/test/clike.conf.dos", clike),
  Mac  = property_file:parse("./priv/test/clike.conf.mac", clike),
  Tests =
    [
     {"Unix", [Unix, Unix], true},
     {"DOS", [Dos, Unix], true},
     {"Mac", [Mac, Unix], true}
    ],

    ?assert(ok == process_tests(fun (List1, List2) -> List1 == List2 end, Tests)).



%%%-------------------------------------------------------------------
%%% Test helpers
%%%-------------------------------------------------------------------
process_tests(Fun, Tests) ->
  process_tests(Fun, Tests, 1).

%% Test argument is in the form {Msg, Args, TrueResult}
%% Msg        - test purpose
%% Args       - [Arg]
%% TrueResult - true result pattern
process_tests(_Fun, [], _) ->
  ok;
process_tests(Fun, [Test|Tail], Num) ->
  {Msg, Args, ResPat} = Test,
  Status = case catch apply(Fun,Args) of
             Res when is_function(ResPat) ->
               case catch ResPat(Res) of
                 ok ->
                   [passed, ok];
                 _ ->
                   [Res, failed]
               end;
             ResPat ->
               [passed, ok];
             Other ->
               [Other, failed]
           end,
  io:format("Test ~2.2w: "++Msg++" -> ~p, ~p~n", [Num|Status]),
  process_tests(Fun, Tail, Num+1).
