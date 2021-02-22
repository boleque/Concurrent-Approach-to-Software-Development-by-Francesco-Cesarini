-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
  register(frequency, spawn(frequency, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%%  The client Functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq, Pid),
      reply(Pid, ok),
      loop(NewFrequencies);
    {request, Pid, stop} ->
        % UPD2: stop if there are not allocated frequencies
        case Frequencies of
            {_, []} ->
                reply(Pid, ok);
            _ -> 
                reply(Pid, {error, allocated_frequencies}),
                loop(Frequencies)
        end
  end.

reply(Pid, Reply) ->
  Pid ! {reply, Reply}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Owner) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|FreeRest]=Free, Allocated}, Owner) ->

    % UPD3: allocate if an owner has not more that 3 frequencies
    FreqCount = lists:foldl(
        fun({_, PID}, Count) when PID == Owner -> Count+1; (_,Count) -> Count end, 
        0, 
        Allocated
    ),

    if 
        FreqCount =< 3 -> 
            {{FreeRest, [{Freq, Owner}|Allocated]}, {ok, Freq}};
        true -> 
            {{Free, Allocated}, {error, exhaust_frequency}}
    end.

%% UPD1: only owner is able to release frequency 
deallocate({Free, Allocated}, Freq, Owner) ->

    NewAllocated = lists:filtermap(
        fun(X) -> 
            case X of 
                {Freq, Owner} -> true; 
                _ -> false 
            end 
        end, 
        Allocated
    ),

    {[Freq|Free],  NewAllocated}.