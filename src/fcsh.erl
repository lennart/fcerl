-module(fcsh).
-export([start/1,compile/1,code_change/3,terminate/2,handle_cast/2,handle_info/2,init/1]).
-behaviour(gen_server).

-record(state, {port,target,tasks, errors, response, compiled}).

start(Pid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [self()], []).

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
  io:format("Processing Command: ~p~nPort: ~p~nAlready Compiled?: ~p~n",[Data,Port,Compiled]),
  case proplists:lookup(Data,Tasks) of
    {Data, Id} ->
      Port ! {self(), {command, lists:flatten(io_lib:format("compile ~s~n",[Id]))}},
      {noreply, #state{port = Port, target = Pid, tasks = Tasks, errors = [], response = [], compiled = Compiled}};
    _ ->

      Port ! {self(), {command, Data}},
      {noreply, #state{port = Port, target = Pid, tasks = [{Data,"1"}|Tasks], response = [], errors = [], compiled = Compiled}}
  end.


handle_info(Info, #state{port = Port, target = Pid, response = Response, errors = Errors, tasks = Tasks, compiled = Compiled} = State) ->
  case Info of
    {Port, {data, Data}} ->
      List = binary_to_list(Data),
      Tokens = string:tokens(List,"\n"),
      {Finished, NewErrors, NewIntermediates} = lists:foldl(
        fun(Token, {Finished, NewErrors, NewIntermediates}) -> 
            case Token of
              [] ->
                {Finished, NewErrors, NewIntermediates};
              "(fcsh) " ->
                {true, NewErrors, NewIntermediates};
              Intermediate ->
                Input = binary_to_list(Data),
                ErrorAt = string:str(Input,"Error:"),
                if
                  ErrorAt > 0 ->
                    io:format("Error encountered~n", []),
                    {Finished, [Data|NewErrors], NewIntermediates};
                  true ->
                    {Finished, NewErrors, [Data|NewIntermediates]}
                end
            end
        end,{false,[],[]},Tokens),
      io:format("Report~nFinished: ~p~nErrors: ~p~nIntermediates: ~p~n",[Finished,NewErrors,NewIntermediates]),
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

