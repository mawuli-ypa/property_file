-module(clike_property_parse).
-file("include/property_parse.hrl", 0).
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id: property_parse.hrl,v 1.1.1.1 2002/10/04 00:00:00 svg Exp $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%
-export([parse/2, parse/3, return_error/2, format_error/1]).

-define(THIS_MODULE, ?MODULE).
-define(STACK_PREFIX, "stack").

parse(FileName, Grammar) ->
  parse(FileName, Grammar, []).

parse(FileName, Grammar, Env) ->
  case catch make_tokenizer(FileName, Grammar) of
    {eof, _} ->
      {ok, eof};
    Error = {error, _} ->
      Error;
    Tokenizer ->
      create_stack(),
      lists:foreach(fun ({N, V}) -> var_add(N, V) end, Env),
      Res = (catch yeccpars1([], Tokenizer, 0, [], [])),
      drop_stack(),
      case Res of
	{ok, Result} ->
	  Result;
	Error ->
	  Error
      end
  end.

format_error({Line, Module, Error}) ->
  format_error(io_lib:format("Line ~p: ~s",
			     [Line, Module:format_error(Error)]));
format_error(Message) ->
  case io_lib:deep_char_list(Message) of
    true ->
      lists:flatten(Message);
    _ ->
      io_lib:write(Message)
  end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
  throw({error, {Line, ?THIS_MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token|Tokens], Tokenizer, State, States, Vstack) ->
  yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	    Tokenizer);
yeccpars1([], Tokenizer, State, States,
	  [DE={decl_end, _, _}, {'$INCLUDE', FileName, Line}|Vstack]) ->
  case tokenizer_add(FileName, Line, Tokenizer) of
    {ok, Tokenizer1} ->
      yeccpars1([], Tokenizer1, State, States, [DE, []|Vstack]);
    Error ->
      Error
  end;
yeccpars1([], Tokenizer, State, States, Vstack) ->
  case tokenizer_next(Tokenizer) of
    {eof, Endline} ->
      return_error(Endline, "end_of_file");
    Error = {error, _} ->
      throw(Error);
    {'EXIT', Reason} ->
      return_error(0, Reason);
    {ok, Tokens, Tokenizer1} ->
      case yeccpars1(Tokens, Tokenizer1, State, States, Vstack) of
	error ->
	  Errorline = element(2, hd(Tokens)),
	  return_error(Errorline, "syntax error at or after this line.");
	Other ->
	  Other
      end
  end.

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?THIS_MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);

yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_simple(Name, Value) when is_list(Value) ->
  VarName = list_to_atom(Name),
  var_add(VarName, Value),
  {VarName, [Value]};
add_simple(Name, Value) when Value == true; Value == false ->
  {list_to_atom(Name), Value};
add_simple(Name, Value) ->
  {list_to_atom(Name), [Value]}.

add_simple_list(Name, [Value]) ->
  add_simple(Name, Value);
add_simple_list(Name, Values) ->
  {list_to_atom(Name), Values}.

verify_struct({Name, _}, {Name, _}) ->
  ok;
verify_struct({_, LineBegin}, {_, LineEnd}) ->
  return_error(LineEnd, io_lib:format("Tag mismatch at line ~w", [LineBegin])).

add_struct(Name, Attrs, Ctxt) ->
  {list_to_atom(Name), Attrs, Ctxt}.

add_op("INCLUDE", FileName, Line) ->
  {'$INCLUDE', FileName, Line};
add_op(Name, _, Line) ->
  return_error(Line, io_lib:format("Invalid operator ~s", [Name])).

add_value([], List) ->
  List;
add_value(Val, List) ->
  [Val|List].

value_of(Token) when is_tuple(Token) ->
  element(3, Token);
value_of(Token) ->
   return_error(0, io_lib:format("Bad token ~p", [Token])).

line_of(Token) ->
  element(2, Token).

%% Var Stack
create_stack() ->
  stack_name().

drop_stack() ->
  stack_name() ! stop.

push_frame() ->
  stack_name() ! {push_frame, self()}.

pop_frame() ->
  stack_name() ! {pop_frame, self()}.

var_add(Name, Value) ->
  stack_name() ! {add_var, self(), {Name, Value}}.

var_value(Name) ->
  Self = self(),
  SN = stack_name(),
  SN ! {var_value, Self, Name},
  receive
    {var_value, SN, {Name, Value}} ->
       Value
  end.

stack_name() ->
  SN = list_to_atom(?STACK_PREFIX ++ pid_to_list(self())),
  case whereis(SN) of
    undefined ->
      SP = spawn_link(fun () -> stack_create(self()) end),
      register(SN, SP),
      SP;
    Pid ->
      Pid
  end.
  
stack_create(Parent) ->
  stack_loop({Parent, [stack_new_frame()]}).

stack_loop({Parent, Stack=[Top|Rest]}) ->
  receive
    {push_frame, _} ->
      NF = stack_new_frame(Top),
      stack_loop({Parent, [NF|Stack]});
    {pop_frame, _} ->
      stack_loop({Parent, Rest});
    {add_var, _, {Name, Value}} ->
      Top1 = gb_trees:enter(Name, Value, Top),
      stack_loop({Parent, [Top1|Rest]});
    {var_value, Pid, Name} ->
      Ret =
	case gb_trees:lookup(Name, Top) of
	  none ->
	    "";
	  {value, Val} ->
	    Val
	end,
      Pid ! {var_value, self(), {Name, Ret}},
      stack_loop({Parent, Stack});
    stop ->
      exit(normal);
    {'EXIT', Parent, Reson} ->
      exit(Reson)
  end.

stack_new_frame() ->
  gb_trees:empty().

stack_new_frame(Top) ->
  _NF = gb_trees:empty(),
  gb_trees:from_orddict(gb_trees:to_list(Top)).

%% Scanner
make_tokenizer(FileName, Grammar) ->
  case scan_file(FileName, Grammar) of
    [] ->
      {eof, 0};
    Error ={error, _} ->
      Error;
    Tokens ->
      {_, EndLine} = lists:last(Tokens),
      TokFun =
	fun (next, {_, [], EL}) ->
	    {eof, EL};
	    (next, {Fun, [T|Ts], EL}) ->
	    {ok, [T], {Fun, Ts, EL}};
	    ({add, FN, Line}, {Fun, Ts, EL}) ->
	    case catch scan_file(FN, Grammar) of
	      {error, Error} ->
		Msg = io_lib:format("Include file ~s ~s",
				    [FN, format_error(Error)]),
		return_error(Line, Msg);
	      AddTs ->
		AddTs1 = lists:sublist(AddTs, length(AddTs)-1),
		{ok, {Fun, AddTs1 ++ Ts, EL}}
	    end
	end,
      {TokFun, Tokens, EndLine}
  end.

tokenizer_next(State={Fun, _, _}) ->	  
  Fun(next, State).

tokenizer_add(FileName, Line, State={Fun, _, _}) ->
  Fun({add, FileName, Line}, State).

scan_file(FileName, {GrammarMod, Grammar}) ->
  _Buf = file_to_buf(FileName),
  AddFName =
    fun ({C, L, V}) -> {C, {FileName, L}, V};
	({C, L}) -> {C, {FileName, L}}
    end,
  GrammarMod:create_env(),
  Res = (catch [AddFName(T) || T <- scan(file_to_buf(FileName), Grammar)]),
  GrammarMod:drop_env(),
  Res.

scan(Buf, Grammar) ->
  case mlex:scan(property_str_buf, Buf, Grammar) of
    Error = {error, _} ->
      throw(Error);
    Res ->
      Res
  end.

file_to_buf(FileName) ->
  case file:read_file(FileName) of
    {ok, Bin} ->
      string_to_buf(Bin);
    Error = {error, _} ->
      throw(Error)
  end.

string_to_buf(Bin) when is_binary(Bin) ->
  string_to_buf(binary_to_list(Bin));
string_to_buf(Str) ->
  property_str_buf:str_to_buf(Str).


-file("src/clike_property_parse.yrl", 147).


-file("src/clike_property_parse.erl", 304).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 10, [S | Ss], [T | Stack]);
yeccpars2_0(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 11, [S | Ss], [T | Stack]);
yeccpars2_0(S, operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 12, [S | Ss], [T | Stack]);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_6(_S, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 16, [S | Ss], [T | Stack]);
yeccpars2_1(S, float_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 29, [S | Ss], [T | Stack]);
yeccpars2_1(S, integer_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 30, [S | Ss], [T | Stack]);
yeccpars2_1(S, ipv4_address, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 31, [S | Ss], [T | Stack]);
yeccpars2_1(S, ipv4_netmask, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 32, [S | Ss], [T | Stack]);
yeccpars2_1(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_1(S, substring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_1(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccpars2_42(_S, Cat, [1 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_2(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 40, [S | Ss], [T | Stack]);
yeccpars2_2(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_3(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 39, [S | Ss], [T | Stack]);
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 38, [S | Ss], [T | Stack]);
yeccpars2_4(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 'yeccgoto_\'<decl>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<config>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 10, [S | Ss], [T | Stack]);
yeccpars2_7(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 11, [S | Ss], [T | Stack]);
yeccpars2_7(S, operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 12, [S | Ss], [T | Stack]);
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccpars2_37(_S, Cat, [7 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_8(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 36, [S | Ss], [T | Stack]);
yeccpars2_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_10(S, assign_op, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_10(_S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_atom_string(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), atom_string, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, float_num, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_float_num(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), float_num, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, integer_num, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_integer_num(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), integer_num, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, ipv4_address, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_ipv4_address(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), ipv4_address, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, ipv4_netmask, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_ipv4_netmask(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), ipv4_netmask, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, l_brace, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_l_brace(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), l_brace, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_string(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), string, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, substring, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_substring(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), substring, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_var_begin(Stack),
 'yeccgoto_\'<struct_name>\''(hd(Ss), var_begin, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 'yeccgoto_\'<bool_decl>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 'yeccgoto_\'<empty_decl>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 16, [S | Ss], [T | Stack]);
yeccpars2_12(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_12(S, substring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_12(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_13(S, r_brace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 22, [S | Ss], [T | Stack]);
yeccpars2_13(S, substring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<string_value>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_15_(Stack),
 'yeccgoto_\'<operator_decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 'yeccgoto_\'<string_value>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 'yeccgoto_\'<string_value>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_19(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 20, [S | Ss], [T | Stack]);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_20_(Stack),
 'yeccgoto_\'<var_val>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 'yeccgoto_\'<string_value>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 'yeccgoto_\'<var_subst>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 'yeccgoto_\'<var_subst>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_24_(Stack),
 'yeccgoto_\'<var_subst>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 16, [S | Ss], [T | Stack]);
yeccpars2_25(S, bool, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 28, [S | Ss], [T | Stack]);
yeccpars2_25(S, float_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 29, [S | Ss], [T | Stack]);
yeccpars2_25(S, integer_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 30, [S | Ss], [T | Stack]);
yeccpars2_25(S, ipv4_address, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 31, [S | Ss], [T | Stack]);
yeccpars2_25(S, ipv4_netmask, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 32, [S | Ss], [T | Stack]);
yeccpars2_25(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_25(S, substring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_25(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<simple_value>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 16, [S | Ss], [T | Stack]);
yeccpars2_27(S, float_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 29, [S | Ss], [T | Stack]);
yeccpars2_27(S, integer_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 30, [S | Ss], [T | Stack]);
yeccpars2_27(S, ipv4_address, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 31, [S | Ss], [T | Stack]);
yeccpars2_27(S, ipv4_netmask, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 32, [S | Ss], [T | Stack]);
yeccpars2_27(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_27(S, substring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_27(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2_33(_S, Cat, [27 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_28_(Stack),
 'yeccgoto_\'<bool_decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 'yeccgoto_\'<simple_value>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 'yeccgoto_\'<simple_value>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 'yeccgoto_\'<simple_value>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 'yeccgoto_\'<simple_value>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_33_(Stack),
 'yeccgoto_\'<simple_decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_34(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 16, [S | Ss], [T | Stack]);
yeccpars2_34(S, float_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 29, [S | Ss], [T | Stack]);
yeccpars2_34(S, integer_num, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 30, [S | Ss], [T | Stack]);
yeccpars2_34(S, ipv4_address, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 31, [S | Ss], [T | Stack]);
yeccpars2_34(S, ipv4_netmask, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 32, [S | Ss], [T | Stack]);
yeccpars2_34(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_34(S, substring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_34(S, var_begin, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccpars2_35(_S, Cat, [34 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_35_(Stack),
 'yeccgoto_\'<simple_values>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_36_(Stack),
 'yeccgoto_\'<decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_37_(Stack),
 'yeccgoto_\'<decls>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_38_(Stack),
 'yeccgoto_\'<decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_39_(Stack),
 'yeccgoto_\'<decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_40_(Stack),
 'yeccgoto_\'<decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_41(S, l_brace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 44, [S | Ss], [T | Stack]);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<struct_attrs>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_43_(Stack),
 'yeccgoto_\'<struct_decl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, atom_string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 10, [S | Ss], [T | Stack]);
yeccpars2_44(S, decl_end, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 11, [S | Ss], [T | Stack]);
yeccpars2_44(S, operator, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 12, [S | Ss], [T | Stack]);
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccpars2_45(45, Cat, [44 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_45(S, r_brace, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 46, [S | Ss], [T | Stack]);
yeccpars2_45(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 'yeccgoto_\'<struct_body>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'<bool_decl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<bool_decl>\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<bool_decl>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<bool_decl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<bool_decl>', State, missing_in_goto_table}}).

'yeccgoto_\'<config>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<config>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<config>', State, missing_in_goto_table}}).

'yeccgoto_\'<decl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<decl>\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<decl>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<decl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<decl>', State, missing_in_goto_table}}).

'yeccgoto_\'<decls>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<decls>\''(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<decls>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(45, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<decls>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<decls>', State, missing_in_goto_table}}).

'yeccgoto_\'<empty_decl>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<empty_decl>\''(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<empty_decl>\''(44=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<empty_decl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<empty_decl>', State, missing_in_goto_table}}).

'yeccgoto_\'<operator_decl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<operator_decl>\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<operator_decl>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<operator_decl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<operator_decl>', State, missing_in_goto_table}}).

'yeccgoto_\'<simple_decl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_decl>\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_decl>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_decl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<simple_decl>', State, missing_in_goto_table}}).

'yeccgoto_\'<simple_value>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_value>\''(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(27, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_value>\''(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_value>\''(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_value>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<simple_value>', State, missing_in_goto_table}}).

'yeccgoto_\'<simple_values>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_values>\''(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_values>\''(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_values>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<simple_values>', State, missing_in_goto_table}}).

'yeccgoto_\'<string_value>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_value>\''(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_value>\''(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_value>\''(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_value>\''(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_value>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<string_value>', State, missing_in_goto_table}}).

'yeccgoto_\'<struct_attrs>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_attrs>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<struct_attrs>', State, missing_in_goto_table}}).

'yeccgoto_\'<struct_body>\''(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_body>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<struct_body>', State, missing_in_goto_table}}).

'yeccgoto_\'<struct_decl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_decl>\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_decl>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_decl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<struct_decl>', State, missing_in_goto_table}}).

'yeccgoto_\'<struct_name>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_name>\''(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_name>\''(44, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_name>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<struct_name>', State, missing_in_goto_table}}).

'yeccgoto_\'<var_subst>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_subst>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<var_subst>', State, missing_in_goto_table}}).

'yeccgoto_\'<var_val>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<var_val>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.4",{'<var_val>', State, missing_in_goto_table}}).

-compile({inline,yeccpars2_0_/1}).
-file("src/clike_property_parse.yrl", 86).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_1_/1}).
-file("src/clike_property_parse.yrl", 109).
yeccpars2_1_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_5_/1}).
-file("src/clike_property_parse.yrl", 92).
yeccpars2_5_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("src/clike_property_parse.yrl", 86).
yeccpars2_7_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_10_atom_string/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_atom_string(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_float_num/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_float_num(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_integer_num/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_integer_num(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_ipv4_address/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_ipv4_address(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_ipv4_netmask/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_ipv4_netmask(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_l_brace/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_l_brace(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_string/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_string(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_substring/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_substring(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_var_begin/1}).
-file("src/clike_property_parse.yrl", 136).
yeccpars2_10_var_begin(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   push_frame ( ) ,
    value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("src/clike_property_parse.yrl", 96).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   add_simple ( value_of ( __1 ) , true )
  end | __Stack].

-compile({inline,yeccpars2_11_/1}).
-file("src/clike_property_parse.yrl", 0).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/clike_property_parse.yrl", 101).
yeccpars2_15_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   add_op ( value_of ( __1 ) , __2 , line_of ( __1 ) )
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/clike_property_parse.yrl", 118).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("src/clike_property_parse.yrl", 117).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("src/clike_property_parse.yrl", 128).
yeccpars2_20_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   var_value ( list_to_atom ( value_of ( __2 ) ) )
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("src/clike_property_parse.yrl", 119).
yeccpars2_21_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 ) ++ __2
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("src/clike_property_parse.yrl", 125).
yeccpars2_22_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("src/clike_property_parse.yrl", 124).
yeccpars2_23_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ value_of ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/clike_property_parse.yrl", 123).
yeccpars2_24_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ value_of ( __2 ) ++ __3
  end | __Stack].

-compile({inline,yeccpars2_27_/1}).
-file("src/clike_property_parse.yrl", 109).
yeccpars2_27_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_28_/1}).
-file("src/clike_property_parse.yrl", 98).
yeccpars2_28_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   add_simple ( value_of ( __1 ) , value_of ( __3 ) )
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("src/clike_property_parse.yrl", 113).
yeccpars2_29_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("src/clike_property_parse.yrl", 112).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_31_/1}).
-file("src/clike_property_parse.yrl", 114).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_32_/1}).
-file("src/clike_property_parse.yrl", 115).
yeccpars2_32_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_33_/1}).
-file("src/clike_property_parse.yrl", 104).
yeccpars2_33_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   add_simple_list ( value_of ( __1 ) , [ __3 | __4 ] )
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("src/clike_property_parse.yrl", 109).
yeccpars2_34_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_35_/1}).
-file("src/clike_property_parse.yrl", 107).
yeccpars2_35_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("src/clike_property_parse.yrl", 88).
yeccpars2_36_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("src/clike_property_parse.yrl", 85).
yeccpars2_37_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   add_value ( __1 , __2 )
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("src/clike_property_parse.yrl", 91).
yeccpars2_38_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("src/clike_property_parse.yrl", 89).
yeccpars2_39_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("src/clike_property_parse.yrl", 90).
yeccpars2_40_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/clike_property_parse.yrl", 131).
yeccpars2_43_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Val = add_struct ( __1 , __2 , __3 ) ,
    pop_frame ( ) ,
    Val
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/clike_property_parse.yrl", 86).
yeccpars2_44_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_46_/1}).
-file("src/clike_property_parse.yrl", 141).
yeccpars2_46_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].


-file("src/clike_property_parse.yrl", 149).
