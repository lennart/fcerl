-module(fcsh).
-export([start/0,compile/1,code_change/3,terminate/2,handle_cast/2,handle_info/2,init/1]).
-behaviour(gen_server).
-define(LOG(Message,Interpolations),io:format(Message,Interpolations)).
-record(state, {port,target,tasks, errors, response, compiled}).

start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [self()], []),
  receive
    {data, [{errors, []},{response, Response}]} ->
      FlattenedResponse = lists:flatten(Response),
      io:format("Response: ~p~n",[FlattenedResponse]),
      FlattenedResponse;
    {data, [{errors, Errors},{response, _Response}]} ->
      FlattenedErrors = lists:flatten(Errors),
      io:format("Error: ~p~n",[FlattenedErrors]),
      {error, FlattenedErrors};
    Else ->
      io:format("This does not match anything: ~p~n",[Else]),
      {error,"Failed"}
  end.

compile(Command) ->
  gen_server:cast(?MODULE,{cmd, [lists:flatten(io_lib:format("~s~n",[Command])),self()]}),
  receive
    {data, [{errors, []},{response, Response}]} ->
      FlattenedResponse = lists:flatten(Response),
      io:format("Response: ~p~n",[FlattenedResponse]),
      FlattenedResponse;
    {data, [{errors, Errors},{response, _Response}]} ->
      FlattenedErrors = lists:flatten(Errors),
      io:format("Error: ~p~n",[FlattenedErrors]),
      {error, FlattenedErrors};
    Else ->
      io:format("This does not match anything: ~p~n",[Else]),
      {error,"Failed"}
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(Reason, #state{port=Port, target=Pid, response=Response, tasks=Tasks, errors=Errors, compiled=Compiled} = State) ->
  port_close(Port).

  

init([Pid]) ->
    Exe = "\"/Applications/Adobe Flash Builder Beta 2/sdks/4.0.0/bin/fcsh\"",
    
    Port = open_port({spawn, Exe}, [binary, stream, stderr_to_stdout]),
    {ok, #state{port=Port, target=Pid, response=[], tasks=[], errors=[], compiled=false}}.

handle_cast({cmd, [Data, Pid]}, #state{port = Port, target = _Pid, response = Response, errors = Errors, tasks= Tasks, compiled = Compiled} = State) ->
  case index_of(Data,Tasks) of
    not_found ->
      io:format("Processing New Command: ~p~nPort: ~p~n",[Data,Port]),
      Port ! {self(), {command, Data}},
      {noreply, #state{port = Port, target = Pid, tasks = lists:append(Tasks,[Data]), response = [], errors = [], compiled = Compiled}};
    Id ->
      io:format("Incremental Compilation for ID: ~B~n",[Id]),
      Port ! {self(), {command, lists:flatten(io_lib:format("compile ~B~n",[Id]))}},
      {noreply, #state{port = Port, target = Pid, tasks = Tasks, errors = [], response = [], compiled = Compiled}}
  end.


handle_info(Info, #state{port = Port, target = Pid, response = Response, errors = Errors, tasks = Tasks, compiled = Compiled} = State) ->
  case Info of
    {Port, {data, Data}} ->
      List = binary_to_list(Data),
      Tokens = string:tokens(List,"\n"),
      {Finished, NewErrors, NewIntermediates} = lists:foldl(
        fun(Token, {_Finished, _Errors, _Intermediates}) -> 
            case Token of
              [] ->
                {_Finished, _Errors, _Intermediates};
              "(fcsh) " ->
                {true, _Errors, _Intermediates};
              Intermediate ->
                ErrorAt = string:str(Token,"Error:"),
                if
                  ErrorAt > 0 ->
                    io:format("Error encountered~n", []),
                    {_Finished, add_if_unique(list_to_binary(Token), _Errors), _Intermediates};
                  true ->
                    {_Finished, _Errors, add_if_unique(list_to_binary(Token),_Intermediates)}
                end
            end
        end,{false,[],[]},Tokens),
      ?LOG("Report~nFinished: ~p~nErrors: ~p~nIntermediates: ~p~n",[Finished,NewErrors,NewIntermediates]),
      if
        Finished ->
          Pid ! {data, [{errors, lists:flatten([NewErrors|Errors])},{response, lists:flatten([NewIntermediates|Response])}]},
          {noreply, #state{port = Port, target = Pid, tasks = Tasks, errors = [], response= [],compiled = true}};
        true ->
          {noreply, #state{port = Port, target = Pid, tasks = Tasks, errors = lists:flatten([NewErrors|Errors]), response= lists:flatten([NewIntermediates|Response]),compiled = true}}
      end;
    Else ->
      io:format("Else: ~p~n", [Else]),
      {noreply, State}
  end.

add_if_unique(E, List) ->
  case lists:any(fun(X) -> X == E end,List) of
    true ->
      List;
    false ->
      [E|List]
  end.

% Taken from Stackoverflow http://stackoverflow.com/questions/1459152/erlang-listsindex-of-function
% by sepp2k
index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

